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
  // val thresholdPip = params.numAddPipes.max(params.numMulPipes) // because of logMode
  val retiming = if (params.retiming) 1 else 0
  val thresholdPipConfig = params.numAddPipes.max(params.numMulPipes) // because of logMode
  val thresholdPip = if (params.logOrLinReg) thresholdPipConfig else if (params.logMode) params.numAddPipes else params.numMulPipes
  val depthOfQueue = if (params.retiming) 1 + thresholdPip else thresholdPip

  //val sumPip = if (params.CFARAlgorithm != GOSCFARType) 2 * params.numAddPipes else 0
  //val latencyComp = params.leadLaggWindowSize + params.guardWindowSize + 1 + sumPip + thresholdPip
  val latencyComp = params.leadLaggWindowSize + params.guardWindowSize + 1 + thresholdPip
  //val latency = io.windowCells +& io.guardCells +& 1.U +& (sumPip + thresholdPip).U
  val latency = io.windowCells +& io.guardCells +& 1.U // +& thresholdPip.U
  assert(io.fftWin > 2.U * io.windowCells + 2.U * io.guardCells + 1.U, "FFT size must be larger than total number of shifting cells inside CFAR core")
  val sumT: T = (io.in.bits * log2Ceil(params.leadLaggWindowSize)).cloneType // to be sure that no overflow occured 
  val sumlagg = RegInit(t = sumT, init = 0.U.asTypeOf(sumT)) // also reset it when state is idle or something similar
  val sumlead = RegInit(t = sumT, init = 0.U.asTypeOf(sumT))
  
  val lisParams = LISParams(
      proto = params.protoIn.cloneType,
      LIStype = "LIS_FIFO",
      LISsize = params.leadLaggWindowSize,
      rtcSize = true,  // use run time configurable lis size
      useSorterEmpty = true,
      useSorterFull = true,
      sortDir = false) // GOS uses descending sorting direction
 
  val laggWindow    = Module(new LinearSorter(lisParams))
  laggWindow.io.in <> io.in
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
  val lastOut = ShiftRegister(cellUnderTest.io.lastOut, depthOfQueue, true.B)

  
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
                   laggSortedData(io.indexLagg.get - 1.U),
                   BinaryRepresentation[T].shr(sumlagg, io.divSum.get))

  // if cfarAlgorithm input is not available then this CFAR is GOSCFARType
  val rightThr = Mux(io.cfarAlgorithm.getOrElse(1.U) === 1.U,
                   leadSortedData(io.indexLead.get - 1.U),
                   BinaryRepresentation[T].shr(sumlead, io.divSum.get)) // this will make a lot of Muxes

  val greatestOf = Mux(leftThr > rightThr, leftThr, rightThr)
  val smallestOf = Mux(leftThr < rightThr, leftThr, rightThr)
  val thrByModes = MuxLookup(io.cfarMode, smallestOf, Array(
    0.U -> BinaryRepresentation[T].shr(rightThr + leftThr, 1),
    1.U -> greatestOf,
    2.U -> smallestOf))

  // when considerEdges = true
  /* val thrWithoutScaling = Mux(!leadWindow.io.sorterFull.get && !laggWindow.io.sorterFull.get,
                            0.U.asTypeOf(sumT),
                            Mux(laggWindow.io.sorterFull.get && !leadWindow.io.sorterFull.get,
                              leftThr,
                              Mux(!laggWindow.io.sorterFull.get && leadWindow.io.sorterFull.get,
                                rightThr,
                                thrByModes)))*/
  // when considerEdges = false
  val enableRightThr = RegInit(false.B)
  when (!(laggWindow.io.sorterFull.get) && laggWindow.io.out.fire()) {
    enableRightThr := true.B
  }
  when (io.lastOut) {
    enableRightThr := false.B
  }
  
  dontTouch(enableRightThr)
  enableRightThr.suggestName("enableRightThr")
  
   val thrWithoutScaling = if (params.retiming == true) 
                            RegNext(Mux(laggWindow.io.sorterFull.get && leadWindow.io.sorterFull.get,
                               thrByModes, Mux(enableRightThr || !leadWindow.io.sorterFull.get, 0.U.asTypeOf(sumT), thrByModes)))
                          else
                             Mux(laggWindow.io.sorterFull.get && leadWindow.io.sorterFull.get,
                               thrByModes, Mux(enableRightThr || !leadWindow.io.sorterFull.get, 0.U.asTypeOf(sumT), thrByModes))
  
  
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

//   val bpos = (threshold.cloneType match {
//     case fp: FixedPoint => fp.binaryPoint.get
//     case _ => 0
//   })
//   println(bpos.toString) // it uses binaryPoint growth
  val cutDelayed = ShiftRegister(cellUnderTest.io.out.bits, depthOfQueue, en = true.B)

  // If peak grouping is enabled it is assumed that guard cells are not equal to zero because it is not possible to extract neighbours (parallel output of lis is sorted sequence)
  val leftNeighb  = ShiftRegister(laggGuard.io.parallelOut(io.guardCells - 1.U), depthOfQueue, en = true.B)
  val rightNeighb = ShiftRegister(leadGuard.io.parallelOut.head, depthOfQueue, en = true.B)
  val isLocalMax = cutDelayed > leftNeighb && cutDelayed > rightNeighb
  val isPeak = cutDelayed  > threshold

  if (params.numAddPipes == 0 && params.numMulPipes == 0) {
    io.out.bits.peak := Mux(io.peakGrouping, isPeak && isLocalMax, isPeak)
    if (params.sendCut)
      io.out.bits.cut.get := cutDelayed
    io.in.ready := ~initialInDone || io.out.ready && ~flushing
    io.out.bits.threshold :=  threshold
    io.out.valid := initialInDone && io.in.fire() || flushing
    io.fftBin := cntOut //fftBinOnOutput
    io.lastOut := cellUnderTest.io.lastOut
  }
  else {
    val queueData = Module(new Queue((io.out.bits.cloneType), depthOfQueue + 1, flow = true))
    io.in.ready := ~initialInDone || io.out.ready && ~flushingDelayed

    queueData.io.enq.valid := ShiftRegister(initialInDone && io.in.fire(), depthOfQueue, en = true.B) || (flushingDelayed && ShiftRegister(io.out.ready, depthOfQueue, en = true.B))
    
    if (params.sendCut)
      queueData.io.enq.bits.cut.get := cutDelayed
    queueData.io.enq.bits.threshold := threshold
    queueData.io.enq.bits.peak := Mux(io.peakGrouping, isPeak && isLocalMax, isPeak)
    queueData.io.deq.ready := io.out.ready

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
