package cfar

import chisel3._
import chisel3.experimental._
import chisel3.util._
import lis._
import dsptools._
import dsptools.numbers._

import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

// core for GOSCACFARType and GOSCFARType
class CFARCoreWithLis[T <: Data : Real : BinaryRepresentation](val params: CFARParams[T]) extends Module {
  require(params.CFARAlgorithm != CACFARType)
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
  val sumT: T = (io.in.bits * log2Ceil(params.leadLaggWindowSize)).cloneType // to be sure that no overflow occured 
  val sumlagg = RegInit(t = sumT, init = 0.U.asTypeOf(sumT)) // also reset it when state is idle or something similar
  val sumlead = RegInit(t = sumT, init = 0.U.asTypeOf(sumT))
  
  val lisParams = LISParams(
      proto = params.protoIn.cloneType,
      LIStype = "LIS_FIFO",
      LISsize = params.leadLaggWindowSize,
      rtcSize = true, // use run time configurable lis size
      useSorterEmpty = true,
      useSorterFull = true,
      sortDir = false) // GOS uses descending sorting direction
 
  val laggWindow    = Module(new LinearSorter(lisParams))
  laggWindow.io.in <> io.in // here check flushing also
  laggWindow.io.lisSize.get := io.windowCells
  laggWindow.io.lastIn := io.lastIn
  val laggSortedData = laggWindow.io.sortedData
  
  val laggGuard     = Module(new AdjustableShiftRegisterStream(params.protoIn.cloneType, params.guardWindowSize))
  laggGuard.io.in <> laggWindow.io.out
  laggGuard.io.lastIn := laggWindow.io.lastOut
  laggGuard.io.depth  := io.guardCells
  
  val cellUnderTest = Module(new CellUnderTest(params.protoIn.cloneType))
  cellUnderTest.io.in <> laggGuard.io.out
  cellUnderTest.io.lastIn := laggGuard.io.lastOut
  //val lastOut = ShiftRegister(cellUnderTest.io.lastOut, sumPip + thresholdPip, io.out.fire())
  //val lastOut = ShiftRegister(cellUnderTest.io.lastOut, thresholdPip, io.out.fire())
  //io.lastOut := lastOut
  
  val leadGuard = Module(new AdjustableShiftRegisterStream(params.protoIn.cloneType, params.guardWindowSize))
  leadGuard.io.in <> cellUnderTest.io.out
  leadGuard.io.depth := io.guardCells
  leadGuard.io.lastIn := cellUnderTest.io.lastOut
  
  val leadWindow    = Module(new LinearSorter(lisParams))
  leadWindow.io.lisSize.get := io.windowCells
  leadWindow.io.in <> leadGuard.io.out
  leadWindow.io.out.ready := Mux(lastCut, true.B, io.out.ready)
  cellUnderTest.io.out.ready := Mux(leadWindow.io.sorterFull.get, leadWindow.io.in.ready, io.out.ready)
  leadWindow.io.lastIn := leadGuard.io.lastOut
  val leadSortedData = leadWindow.io.sortedData
  

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
  
  when (io.lastOut) {
    lastCut := true.B
    flushing := false.B
    initialInDone := false.B
  }
  when (leadWindow.io.sorterEmpty.get === true.B) {
    lastCut := false.B
  }
  
  when (io.lastOut) {
    sumlagg := 0.U.asTypeOf(sumT)
  }
  .elsewhen (io.in.fire()) {
    when (laggWindow.io.sorterFull.get) {
      when (laggWindow.io.out.fire()) {
        sumlagg := sumlagg + laggWindow.io.in.bits - laggWindow.io.out.bits
      }
    }
    .otherwise {
      sumlagg := sumlagg + laggWindow.io.in.bits
    }
  }
 // dontTouch(sumlagg)
 
  when (lastCut) {
    sumlead := 0.U.asTypeOf(sumT)
  }
  .elsewhen (leadWindow.io.in.fire()) {
    when (leadWindow.io.sorterFull.get) {
      when (leadWindow.io.out.fire()) {
        sumlead := sumlead + leadWindow.io.in.bits - leadWindow.io.out.bits
      }
    }
    .otherwise {
      sumlead := sumlead + leadWindow.io.in.bits
    }
  }
  
  val leftThr  = Mux(io.cfarAlgorithm.getOrElse(1.U) === 1.U,
                   laggSortedData(io.indexLagg.get),
                   BinaryRepresentation[T].shr(sumlagg, io.divSum.get))

  // if cfarAlgorithm input is not available then this CFAR is GOSCFARType
  val rightThr = Mux(io.cfarAlgorithm.getOrElse(1.U) === 1.U,
                   leadSortedData(io.indexLead.get),
                   BinaryRepresentation[T].shr(sumlead, io.divSum.get)) // this will make a lot of Muxes

  val greatestOf = Mux(leftThr > rightThr, leftThr, rightThr)
  val smallestOf = Mux(leftThr < rightThr, leftThr, rightThr)
  val thrByModes = MuxLookup(io.cfarMode, smallestOf, Array(
    0.U -> BinaryRepresentation[T].shr(rightThr + leftThr, 1),
    1.U -> greatestOf,
    2.U -> smallestOf))

  val thrWithoutScaling = Mux(!leadWindow.io.sorterFull.get && !laggWindow.io.sorterFull.get,
                            0.U.asTypeOf(sumT),
                            Mux(laggWindow.io.sorterFull.get && !leadWindow.io.sorterFull.get,
                              leftThr,
                              Mux(!laggWindow.io.sorterFull.get && leadWindow.io.sorterFull.get,
                                rightThr,
                                thrByModes)))
  
  val threshold = DspContext.alter(DspContext.current.copy(
    numAddPipes = thresholdPip,
    numMulPipes = thresholdPip)) {
      //thrWithoutScaling context_* io.thresholdScaler
      Mux(io.logOrLinearMode, thrWithoutScaling context_* io.thresholdScaler, thrWithoutScaling context_+ io.thresholdScaler)
  }

//   val bpos = (threshold.cloneType match {
//     case fp: FixedPoint => fp.binaryPoint.get
//     case _ => 0
//   })
//   println(bpos.toString) // it uses binaryPoint growth
  val cutDelayed = ShiftRegister(cellUnderTest.io.out.bits, thresholdPip, en = true.B) //en = cellUnderTest.io.out.fire() || (flushing && io.out.ready))
  
  val leftNeighb  = ShiftRegister(laggGuard.io.parallelOut(io.guardCells - 1), thresholdPip, en = true.B)
  val rightNeighb = ShiftRegister(leadGuard.io.parallelOut(0), thresholdPip, en = true.B)
  val isLeftPeak  = leftNeighb  > threshold //consider here pipes!
  val isRightPeak = rightNeighb > threshold
  val isPeak      = cutDelayed  > threshold
  // no pipes
  io.in.ready := ~initialInDone || io.out.ready && ~flushing
  if (params.numAddPipes == 0 && params.numMulPipes == 0) {
    //io.out.bits.peak := cutDelayed > threshold
    io.out.bits.peak := Mux(io.peakGrouping, isPeak && ~isLeftPeak && ~isRightPeak, isPeak)
    io.out.bits.cut  := cutDelayed
    io.out.bits.threshold :=  threshold
    io.out.valid := initialInDone && io.in.fire() || flushing
    io.fftBin := cntOut //fftBinOnOutput
    io.lastOut := cellUnderTest.io.lastOut
  }
  else {
    val outQueue = Module(new Queue(chiselTypeOf(io.out.bits), entries = thresholdPip, pipe = false, flow = true))
    //outQueue.io.enq.bits.peak := cutDelayed > threshold
    outQueue.io.enq.bits.peak := Mux(io.peakGrouping, isPeak && ~isLeftPeak && ~isRightPeak, isPeak) 
    
    outQueue.io.enq.bits.cut := cutDelayed
    outQueue.io.enq.bits.threshold := threshold // trim here
    outQueue.io.enq.valid := initialInDone && ShiftRegister(io.in.fire(), thresholdPip, false.B, true.B) || flushing
    outQueue.io.deq.ready := io.out.ready
    
//     //val outBinQueue = Module(new Queue(chiselTypeOf(io.fftBin), entries = sumPip + thresholdPip, pipe = true, flow = true))
//     val outBinQueue = Module(new Queue(chiselTypeOf(io.fftBin), entries = thresholdPip, pipe = false, flow = true))
//     outBinQueue.io.enq.bits := cntOut //fftBinOnOutput
//     outBinQueue.io.enq.valid := initialInDone && ShiftRegister(io.in.fire(), thresholdPip, false.B, true.B) || (flushing && ~io.lastOut) //(flushing && ~io.lastOut && io.out.ready)
//     outBinQueue.io.deq.ready := io.out.ready
    io.fftBin := cntOut
    val outLastQueue = Module(new Queue(Bool(), entries = thresholdPip, pipe = false, flow = true))
    
    outLastQueue.io.enq.bits  := ShiftRegister(cellUnderTest.io.lastOut, thresholdPip, true.B)
    outLastQueue.io.enq.valid := initialInDone && ShiftRegister(io.in.fire(), thresholdPip, false.B, true.B) || flushing
    outLastQueue.io.deq.ready := io.out.ready
  //  assert(outQueue.io.deq.valid === outBinQueue.io.deq.valid, "Must be asserted at the same time")
    
    io.lastOut := outLastQueue.io.deq.bits
    io.out.bits.peak := outQueue.io.deq.bits.peak
    io.out.bits.cut  := outQueue.io.deq.bits.cut
    io.out.bits.threshold := outQueue.io.deq.bits.threshold
    io.out.valid := outQueue.io.deq.valid
  }
}

object CFARCoreWithLisApp extends App
{
  // just simple test
  val params: CFARParams[FixedPoint] =  CFARParams(
    protoIn = FixedPoint(16.W, 8.BP),
    protoThreshold = FixedPoint(16.W, 8.BP),
    protoScaler = FixedPoint(16.W, 8.BP)
   // other parameters are default
  )
  chisel3.Driver.execute(args,()=>new CFARCoreWithLis(params))
}
