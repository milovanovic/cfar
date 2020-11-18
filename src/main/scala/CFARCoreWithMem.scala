package cfar

import chisel3._
import chisel3.experimental._
import chisel3.util._

import dsptools._
import dsptools.numbers._

import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

// core for CACFARType
class CFARCoreWithMem[T <: Data : Real : BinaryRepresentation](val params: CFARParams[T]) extends Module {
  require(params.CFARAlgorithm == CACFARType)
  
  val io = IO(CFARIO(params))
  // for suporting peakGrouping AdjustableShiftRegisterStream needs to have parallel outputs!
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
  val sumT: T = (io.in.bits * log2Ceil(params.leadLaggWindowSize)).cloneType // to be sure that no overflow occured 
  val sumlagg = RegInit(t = sumT, init = 0.U.asTypeOf(sumT)) // also reset it when state is idle or something similar
  val sumlead = RegInit(t = sumT, init = 0.U.asTypeOf(sumT))
  
  val laggWindow    =  Module(new ShiftRegisterMemStream(params.protoIn, params.leadLaggWindowSize)) //Module(new LinearSorter(lisParams))
  laggWindow.io.in <> io.in // here check flushing also
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
  
  //BinaryRepresentation[T].shr(v, 1)
  val thrByModes = MuxLookup(io.cfarMode, smallestOf, Array(
    0.U -> BinaryRepresentation[T].shr(rightThr + leftThr, 1),
    1.U -> greatestOf,
    2.U -> smallestOf))
  
  val thrWithoutScaling = Mux(!leadWindow.io.memFull && !laggWindow.io.memFull,
                          0.U.asTypeOf(sumT),
                          Mux(laggWindow.io.memFull && !leadWindow.io.memFull,
                            leftThr,
                            Mux(!laggWindow.io.memFull && leadWindow.io.memFull,
                              rightThr,
                              thrByModes)))

  val threshold = DspContext.alter(DspContext.current.copy(
    numAddPipes = thresholdPip,
    numMulPipes = thresholdPip)) {
      Mux(io.logOrLinearMode, thrWithoutScaling context_* io.thresholdScaler, thrWithoutScaling context_+ io.thresholdScaler)
  }
  //val cutDelayed = ShiftRegister(cellUnderTest.io.out.bits, sumPip + thresholdPip, en = cellUnderTest.io.out.fire() || (flushing && io.out.ready))
  val cutDelayed = ShiftRegister(cellUnderTest.io.out.bits, thresholdPip, en = cellUnderTest.io.out.fire() || (flushing && io.out.ready))
  
  // initialInDone is maybe going to be Shifted by the number of the pipeline registers
  io.in.ready := ~initialInDone || io.out.ready && ~flushing
  
  // peak peakGrouping
  val leftNeighb  = ShiftRegister(laggGuard.io.parallelOut(io.guardCells - 1), thresholdPip, en = true.B)
  val rightNeighb = ShiftRegister(leadGuard.io.parallelOut(0), thresholdPip, en = true.B)
  val isLeftPeak  = leftNeighb  > threshold //consider here pipes
  val isRightPeak = rightNeighb > threshold
  val isPeak      = cutDelayed  > threshold
  // no pipes
  if (params.numAddPipes == 0 && params.numMulPipes == 0) {
    //io.out.bits.peak := cutDelayed > threshold
    io.out.bits.peak := Mux(io.peakGrouping, isPeak && ~isLeftPeak && ~isRightPeak, isPeak)
    io.out.bits.cut  := cutDelayed
    io.out.bits.threshold :=  threshold
    io.out.valid := initialInDone && io.in.fire() || flushing
    io.fftBin := cntOut //fftBinOnOutput
  }
  else {
    io.out.bits.peak := Mux(io.peakGrouping, isPeak && ~isLeftPeak && ~isRightPeak, isPeak)
    io.out.bits.cut  := cutDelayed
    io.out.bits.threshold :=  threshold
    io.out.valid := initialInDone && io.in.fire() || flushing
    io.fftBin := cntOut //fftBinOnOutput
/*    // How to understand this
    val skidIn = Wire(io.out.cloneType)
    skidIn.bits.cut  := io.in.bits.cut
    skidIn.bits.peak := io.in.bits.peak
    skidIn.bits.threshold := threshold
    skidIn.valid := initialInDone && io.in.fire() || (flushing && io.out.ready) *///in.valid
    
  //  io.in.ready := skidIn.ready //Mux(estimatingDone, skidIn.ready, estimator.in.ready)
    
    
    // let's try to make this with skid buffer and check utilization for that
  
//     // this part here should be the same as it is inside CFARCoreWithLis
//     //val outQueue =  Module(new Queue(chiselTypeOf(io.out.bits), entries = sumPip + thresholdPip, pipe = true, flow = true)) // +1?
//     val outQueue = Module(new Queue(chiselTypeOf(io.out.bits), entries = thresholdPip, pipe = true, flow = true))
//     //outQueue.io.enq.bits.peak := cutDelayed > threshold
//     outQueue.io.enq.bits.peak := Mux(io.peakGrouping, isPeak && ~isLeftPeak && ~isRightPeak, isPeak) //cutDelayed > threshold
//     
//     outQueue.io.enq.bits.cut  := cutDelayed
//     outQueue.io.enq.bits.threshold := threshold // trim here
//     outQueue.io.enq.valid := initialInDone && io.in.fire() || (flushing && io.out.ready)
//     outQueue.io.deq.ready := io.out.ready
//     
//    /*
//     val outBinQueue = Module(new Queue(chiselTypeOf(io.fftBin), entries = thresholdPip, pipe = true, flow = true))
//     outBinQueue.io.enq.bits := fftBinOnOutput
//     outBinQueue.io.enq.valid := initialInDone && io.in.fire() || (flushing && io.out.ready)
//     outBinQueue.io.deq.ready := io.out.ready*/
//     io.fftBin := cntOut //outBinQueue.io.deq.bits
//     //assert(outQueue.io.deq.valid === outBinQueue.io.deq.valid, "Must be asserted at the same time")
//     
//     io.out.bits.peak := outQueue.io.deq.bits.peak
//     io.out.bits.cut  := outQueue.io.deq.bits.cut
//     io.out.bits.threshold := outQueue.io.deq.bits.threshold
//     io.out.valid := outQueue.io.deq.valid
  }
}

object CFARCoreWithMemApp extends App
{
  // just simple test
  val params: CFARParams[FixedPoint] =  CFARParams(
    protoIn = FixedPoint(16.W, 8.BP),
    protoThreshold = FixedPoint(16.W, 8.BP), // output thres
    protoScaler = FixedPoint(16.W, 8.BP),
    CFARAlgorithm = CACFARType
   // other parameters are default
  )
  
  chisel3.Driver.execute(args,()=>new CFARCoreWithMem(params))
}
