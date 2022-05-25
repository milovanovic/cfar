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
  val thresholdPipConfig = params.numAddPipes.max(params.numMulPipes) // because of logMode
  val thresholdPip = if (params.logOrLinReg) thresholdPipConfig else if (params.logMode) params.numAddPipes else params.numMulPipes
  
  val latencyComp = params.leadLaggWindowSize + params.guardWindowSize + 1 + thresholdPip
  //val latency = io.windowCells +& io.guardCells +& 1.U +& (sumPip + thresholdPip).U
  val retiming = if (params.retiming) 1 else 0
  val depthOfQueue = if (params.retiming) 1 + thresholdPip else thresholdPip
  val latency = io.windowCells +& io.guardCells +& 1.U //+& thresholdPip.U // + retimingInt
  assert(io.fftWin > 2.U * io.windowCells + 2.U * io.guardCells + 1.U, "FFT size must be larger than total number of shifting cells inside CFAR core")
  assert(io.guardCells > 0.U, "Number of guard cells should be greater than 0")
  
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
  val lastOut = ShiftRegister(cellUnderTest.io.lastOut, depthOfQueue, true.B)
  
  val leadGuard = Module(new AdjustableShiftRegisterStream(params.protoIn.cloneType, params.guardWindowSize))
  leadGuard.io.in <> cellUnderTest.io.out
  leadGuard.io.depth := io.guardCells
  leadGuard.io.lastIn := cellUnderTest.io.lastOut
  
  //////////////// ADDED ///////////////
  leadGuard.io.out.ready := io.out.ready
  
  val leadWindow    = Module(new ShiftRegisterMemStream(params.protoIn, params.leadLaggWindowSize, enInitStore = false))
  leadWindow.io.depth := io.windowCells
  leadWindow.io.in <> leadGuard.io.out
  //leadWindow.io.out.ready := Mux(lastCut, true.B, io.out.ready)
  
  ////////////////////// CHANGED ////////////////////////
  leadWindow.io.out.ready := io.out.ready
  
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
  
  val fftBinOnOutput = cntOut
 
  when (io.lastIn && io.in.valid) {
    flushing := true.B
  }
  val flushingDelayed = ShiftRegisterWithReset(in = flushing,
                                                n = thresholdPip + retiming,
                                                resetData = false.B,
                                                reset = io.lastOut,
                                                en = true.B)
  when (io.lastOut) {
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
  
  val thrWithoutScaling = if (params.retiming == true) 
                            RegNext(Mux(!leadWindow.io.memFull && !laggWindow.io.memFull,
                              0.U.asTypeOf(sumT),
                              Mux(laggWindow.io.memFull && !leadWindow.io.memFull,
                                leftThr,
                                //Mux((!(laggWindow.io.memFull) && leadWindow.io.memFull,
                                Mux(enableRightThr,
                                  rightThr,
                                  thrByModes))))
                          else
                            Mux(!leadWindow.io.memFull && !laggWindow.io.memFull,
                              0.U.asTypeOf(sumT),
                              Mux(laggWindow.io.memFull && !leadWindow.io.memFull,
                                leftThr,
                                //Mux((!(laggWindow.io.memFull) && leadWindow.io.memFull,
                                Mux(enableRightThr,
                                  rightThr,
                                  thrByModes)))

  val threshold = if (params.logOrLinReg) DspContext.alter(DspContext.current.copy(
    numAddPipes = thresholdPip,
    numMulPipes = thresholdPip)) {
      Mux(io.logOrLinearMode.get, thrWithoutScaling context_* io.thresholdScaler, thrWithoutScaling context_+ io.thresholdScaler)
    }
    else if (!params.logMode)
      DspContext.withNumMulPipes(params.numMulPipes) {
        thrWithoutScaling context_* io.thresholdScaler
      }
    else
      DspContext.withNumAddPipes(params.numAddPipes) {
        thrWithoutScaling context_+ io.thresholdScaler
      }

  val cutDelayed = ShiftRegister(cellUnderTest.io.out.bits, depthOfQueue, en = true.B)
  // Important: it is assumed that guard window can not be configured to be zero
  val leftNeighb  = ShiftRegister(laggGuard.io.parallelOut(io.guardCells - 1.U), depthOfQueue, en = true.B)
  val rightNeighb = ShiftRegister(laggGuard.io.parallelOut.head, depthOfQueue, en = true.B)
  val isLocalMax = cutDelayed > leftNeighb && cutDelayed > rightNeighb
  val isPeak = cutDelayed  > threshold
 
  if (params.numAddPipes == 0 && params.numMulPipes == 0) {
    //io.out.bits.peak := cutDelayed > threshold
    io.out.bits.peak := Mux(io.peakGrouping, isPeak && isLocalMax, isPeak)
    if (params.sendCut)
      io.out.bits.cut.get := cutDelayed
    io.in.ready := ~initialInDone || io.out.ready && ~flushing
    io.lastOut := lastOut
    io.out.bits.threshold :=  threshold
    io.out.valid := initialInDone && io.in.fire() || flushing
    io.fftBin := cntOut
  }
  else {
    val queueData = Module(new Queue((io.out.bits.cloneType), depthOfQueue + 1, flow = true))
    val queueFftBin = Module(new Queue((cntOut.cloneType), depthOfQueue + 1, flow = true))
    io.in.ready := ~initialInDone || io.out.ready && ~flushingDelayed

    queueData.io.enq.valid := ShiftRegister(initialInDone && io.in.fire(), depthOfQueue, en = true.B) || (flushingDelayed && ShiftRegister(io.out.ready, depthOfQueue, en = true.B))
    
    if (params.sendCut)
      queueData.io.enq.bits.cut.get := cutDelayed
    queueData.io.enq.bits.threshold := threshold
    queueData.io.enq.bits.peak := Mux(io.peakGrouping, isPeak && isLocalMax, isPeak)
    queueData.io.deq.ready := io.out.ready

    queueFftBin.io.enq.valid := ShiftRegister(initialInDone && io.in.fire(), depthOfQueue, en = true.B)  || (flushingDelayed && ShiftRegister(io.out.ready, depthOfQueue, en = true.B))
    queueFftBin.io.enq.bits := cntOut
    queueFftBin.io.deq.ready := io.out.ready

    val queueLast = Module(new Queue(Bool(), depthOfQueue + 1, flow = true))
    queueLast.io.enq.valid := ShiftRegister(initialInDone && io.in.fire(), depthOfQueue, en = true.B) || (flushingDelayed && ShiftRegister(io.out.ready, depthOfQueue, en = true.B))
    queueLast.io.enq.bits := lastOut
    
    queueLast.io.deq.ready := io.out.ready
    io.lastOut := queueLast.io.deq.bits
    
    if (params.sendCut)
      io.out.bits.cut.get := queueData.io.deq.bits.cut.get
    io.out.bits.peak := queueData.io.deq.bits.peak
    io.out.bits.threshold := queueData.io.deq.bits.threshold
    io.out.valid := queueData.io.deq.valid
    io.fftBin := cntOut//queueFftBin.io.deq.bits
  }
}

object CFARCoreWithMemApp extends App
{
  val params: CFARParams[FixedPoint] = CFARParams(
    protoIn = FixedPoint(16.W, 12.BP),
    protoThreshold = FixedPoint(16.W, 12.BP),
    protoScaler = FixedPoint(16.W, 12.BP),
    leadLaggWindowSize = 64,
    guardWindowSize = 4,
    sendCut = false,
    numMulPipes = 0,
    fftSize = 1024,
    minSubWindowSize = None,
    includeCASH = false,
    CFARAlgorithm = CACFARType
  )
  
  chisel3.Driver.execute(args,()=>new CFARCoreWithMem(params))
}

