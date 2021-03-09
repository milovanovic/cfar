package cfar

import chisel3._
import chisel3.experimental._
import chisel3.util._

import dsptools._
import dsptools.numbers._

import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

// Implements CFAR core that can support cell averaging (CA), greatest of (GO), smallest of CFAR (SO)
// Leading/lagging windows are implemented via BRAM/SRAM

class CFARCoreWithMem[T <: Data : Real : BinaryRepresentation](val params: CFARParams[T]) extends Module {
  require(params.CFARAlgorithm == CACFARType)
  
  val io = IO(CFARIO(params))
  val lastCut = RegInit(false.B)
  val flushing = RegInit(false.B)
  val cntIn = RegInit(0.U(log2Ceil(params.fftSize).W))
  val cntOut = RegInit(0.U(log2Ceil(params.fftSize).W))
  val initialInDone = RegInit(false.B)
  val thresholdPip = params.numAddPipes.max(params.numMulPipes) // because of logMode
  //val sumPip = if (params.CFARAlgorithm != GOSCFARType) 2 * params.numAddPipes else 0
  //val latencyComp = params.leadLaggWindowSize + params.guardWindowSize + 1 + sumPip + thresholdPip
  val latencyComp = params.leadLaggWindowSize + params.guardWindowSize + 1 + thresholdPip
  //val latency = io.windowCells +& io.guardCells +& 1.U +& (sumPip + thresholdPip).U
  val latency = io.windowCells +& io.guardCells +& 1.U +& thresholdPip.U
  assert(io.fftWin > 2.U * io.windowCells + 2.U * io.guardCells + 1.U, "FFT size must be larger than total number of shifting cells inside CFAR core")
  val sumT: T = (io.in.bits * log2Ceil(params.leadLaggWindowSize)).cloneType // no overflow
  val sumlagg = RegInit(t = sumT, init = 0.U.asTypeOf(sumT))
  val sumlead = RegInit(t = sumT, init = 0.U.asTypeOf(sumT))
  val laggWindow    =  Module(new ShiftRegisterMemStream(params.protoIn, params.leadLaggWindowSize))
  laggWindow.io.in <> io.in
  laggWindow.io.depth := io.windowCells
  laggWindow.io.lastIn := io.lastIn
  
  val laggGuard     = Module(new AdjustableShiftRegisterStream(params.protoIn.cloneType, params.guardWindowSize))
  laggGuard.io.in <> laggWindow.io.out
  laggGuard.io.lastIn := laggWindow.io.lastOut
  laggGuard.io.depth  := io.guardCells
  
  val cellUnderTest = Module(new CellUnderTest(params.protoIn.cloneType))
  cellUnderTest.io.in <> laggGuard.io.out
  cellUnderTest.io.lastIn := laggGuard.io.lastOut
  //val lastOut = ShiftRegister(cellUnderTest.io.lastOut, sumPip + thresholdPip, io.out.fire())
  val lastOut = ShiftRegister(cellUnderTest.io.lastOut, thresholdPip, io.out.fire())
  io.lastOut := lastOut
  
  val leadGuard = Module(new AdjustableShiftRegisterStream(params.protoIn.cloneType, params.guardWindowSize))
  leadGuard.io.in <> cellUnderTest.io.out
  leadGuard.io.depth := io.guardCells
  leadGuard.io.lastIn := cellUnderTest.io.lastOut
  
  val leadWindow    = Module(new ShiftRegisterMemStream(params.protoIn, params.leadLaggWindowSize))
  leadWindow.io.depth := io.windowCells
  leadWindow.io.in <> leadGuard.io.out
  leadWindow.io.out.ready := Mux(lastCut, true.B, io.out.ready)
  
  cellUnderTest.io.out.ready := Mux(leadWindow.io.memFull, leadWindow.io.in.ready, io.out.ready)
  leadWindow.io.lastIn := leadGuard.io.lastOut
  

  when (io.in.fire()) {
    cntIn := cntIn + 1.U
  }
  when (cntIn === (latency - 1.U) && io.in.fire()) {
    initialInDone := true.B
  }
 
  when (io.lastOut && io.out.fire()) {
    cntIn := 0.U
  }
  when (io.out.fire()) {
    cntOut := cntOut + 1.U
  }
  when (cntOut === (io.fftWin - 1.U) && io.out.fire() || (io.lastOut && io.out.fire)) {
    cntOut := 0.U
  }
  
  val fftBinOnOutput = cntOut //cntIn - latency
 
  when (io.lastIn) {
    flushing := true.B
  }
  
  when (lastOut) {
    lastCut := true.B
    flushing := false.B
    initialInDone := false.B
  }
  when (leadWindow.io.memEmpty === true.B) {
    lastCut := false.B
  }
  
  when (io.lastOut) {
    sumlagg := 0.U.asTypeOf(sumT)
  }
  .elsewhen (io.in.fire()) {
    when (laggWindow.io.memFull) {
      when (laggWindow.io.out.fire()) {
        sumlagg := sumlagg + laggWindow.io.in.bits - laggWindow.io.out.bits
      }
    }
    .otherwise {
      sumlagg := sumlagg + laggWindow.io.in.bits
    }
  }
  dontTouch(sumlagg)
 
  when (lastCut) {
    sumlead := 0.U.asTypeOf(sumT)
  }
  .elsewhen (leadWindow.io.in.fire()) {
    when (leadWindow.io.memFull) {
      when (leadWindow.io.out.fire()) {
        sumlead := sumlead + leadWindow.io.in.bits - leadWindow.io.out.bits
      }
    }
    .otherwise {
      sumlead := sumlead + leadWindow.io.in.bits
    }
  }
  dontTouch(sumlead)
  
  val leftThr    = BinaryRepresentation[T].shr(sumlagg, io.divSum.get)
  val rightThr   = BinaryRepresentation[T].shr(sumlead, io.divSum.get)
  
  val greatestOf = Mux(leftThr > rightThr, leftThr, rightThr)
  val smallestOf = Mux(leftThr < rightThr, leftThr, rightThr)
  
  val thrByModes = MuxLookup(io.cfarMode, smallestOf, Array(
    0.U -> BinaryRepresentation[T].shr(rightThr + leftThr, 1),
    1.U -> greatestOf,
    2.U -> smallestOf))

  val enableRightThr = RegInit(false.B)
  when (!(laggWindow.io.memFull) && laggWindow.io.out.fire()) {
    enableRightThr := true.B
  }
  when (io.lastOut) {
    enableRightThr := false.B
  }

  dontTouch(enableRightThr)
  enableRightThr.suggestName("enableRightThr")

  val thrWithoutScaling = Mux(!leadWindow.io.memFull && !laggWindow.io.memFull,
                          0.U.asTypeOf(sumT),
                          Mux(laggWindow.io.memFull && !leadWindow.io.memFull,
                            leftThr,
                            //Mux((!(laggWindow.io.memFull) && leadWindow.io.memFull,
                            Mux(enableRightThr,
                              rightThr,
                              thrByModes)))

  val threshold = DspContext.alter(DspContext.current.copy(
    numAddPipes = thresholdPip,
    numMulPipes = thresholdPip)) {
      Mux(io.logOrLinearMode, thrWithoutScaling context_* io.thresholdScaler, thrWithoutScaling context_+ io.thresholdScaler)
  }
  val cutDelayed = ShiftRegister(cellUnderTest.io.out.bits, thresholdPip, en = cellUnderTest.io.out.fire() || (flushing && io.out.ready))
  
  io.in.ready := ~initialInDone || io.out.ready && ~flushing
  assert(io.guardCells > 0.U, "Number of guard cells should be greater than 0")

  val leftNeighb  = ShiftRegister(laggGuard.io.parallelOut.last, thresholdPip, en = true.B)
  val rightNeighb = ShiftRegister(laggGuard.io.parallelOut.head, thresholdPip, en = true.B)
  val isLocalMax = cutDelayed > leftNeighb && cutDelayed > rightNeighb
  val isPeak = cutDelayed  > threshold
 
  if (params.numAddPipes == 0 && params.numMulPipes == 0) {
    //io.out.bits.peak := cutDelayed > threshold
    io.out.bits.peak := Mux(io.peakGrouping, isPeak && isLocalMax, isPeak)
    if (params.sendCut)
      io.out.bits.cut.get := cutDelayed
    //io.out.bits.cut  := cutDelayed
    io.out.bits.threshold :=  threshold
    io.out.valid := initialInDone && io.in.fire() || flushing
    io.fftBin := cntOut
  }
  else {
    // TODO: Handle logic for pipeline registers
    io.out.bits.peak := Mux(io.peakGrouping, isPeak && isLocalMax, isPeak)
    if (params.sendCut)
      io.out.bits.cut.get := cutDelayed
    io.out.bits.threshold :=  threshold
    io.out.valid := initialInDone && io.in.fire() || flushing
    io.fftBin := cntOut
  }
}

object CFARCoreWithMemApp extends App
{
  val params: CFARParams[FixedPoint] = CFARParams(
    protoIn = FixedPoint(24.W, 8.BP),
    protoThreshold = FixedPoint(16.W, 8.BP),
    protoScaler = FixedPoint(16.W, 8.BP),
    leadLaggWindowSize = 128,
    fftSize = 1024,
    guardWindowSize = 4,
    CFARAlgorithm = CACFARType
   // other parameters have default values defined in case class CFARParams
  )
  
  chisel3.Driver.execute(args,()=>new CFARCoreWithMem(params))
}
