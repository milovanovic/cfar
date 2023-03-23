package cfar

import chisel3._
import chisel3.experimental._
import chisel3.util._
import lis._
import dsptools._
import dsptools.numbers._

import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

// core for GOSCACFARType and GOSCFARType
class CFARCoreOS[T <: Data : Real : BinaryRepresentation](val params: CFARParams[T]) extends Module with HasIO {
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
  val latency = io.windowCells + 1.U //io.windowCells * 2.U + 1.U //+& 1.U
  assert(io.fftWin > 2.U * io.windowCells + 2.U * io.guardCells + 1.U, "FFT size must be larger than total number of shifting cells inside CFAR core")

  val lisParams = LISParams(
      proto = params.protoIn.cloneType,
      LIStype = "LIS_SR", //params.lisFIFOType.get,
      LISsubType = "LIS_FIFO",
      LISsize = params.leadLaggWindowSize*2 + 1, // + 1 is cut, no guard cells
      rtcSize = true,
      sendMiddle = true,
      enInitStore = false,
      useSorterEmpty = true,
      useSorterFull = true,
      sortDir = false) // OS uses descending sorting direction

  val cfarWindow = Module(new LinearSorter(lisParams))
  cfarWindow.io.in <> io.in
  cfarWindow.io.lisSize.get := (io.windowCells * 2.U) + 1.U
  cfarWindow.io.lastIn := io.lastIn
  val sortedData = cfarWindow.io.sortedData

  val cut = cfarWindow.io.middleData.get // cfarWindow.io.out.bits
  val thrData =  sortedData(io.indexOS.get - 1.U).asTypeOf(cfarWindow.io.in.bits.cloneType) //Mux(cut === sortedData(io.indexOS.get - 1.U).asTypeOf(cfarWindow.io.in.bits.cloneType), sortedData(io.indexOS.get).asTypeOf(cfarWindow.io.in.bits.cloneType), sortedData(io.indexOS.get - 1.U).asTypeOf(cfarWindow.io.in.bits.cloneType))

  val lastOut = ShiftRegister(cfarWindow.io.lastOut, depthOfQueue, true.B)

  when (io.in.fire) {
    cntIn := cntIn + 1.U
  }

  when (cntIn === (latency - 1.U) && io.in.fire) {
    initialInDone := true.B
  }

  when (io.lastOut && io.out.fire) {
    cntIn := 0.U
  }

  when (io.out.fire) {
    cntOut := cntOut + 1.U
  }
  when (cntOut === (io.fftWin - 1.U) && io.out.fire || (io.lastOut && io.out.fire)) {
    cntOut := 0.U
  }

  val fftBinOnOutput = cntOut

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

  when (cfarWindow.io.sorterEmpty.get === true.B) {
    lastCut := false.B
  }

  val thrWithoutScaling = if (params.retiming == true)
                             RegNext(Mux(cfarWindow.io.sorterFull.get, thrData, 0.U.asTypeOf(cfarWindow.io.in.bits)))
                          else
                             Mux(cfarWindow.io.sorterFull.get, thrData, 0.U.asTypeOf(cfarWindow.io.in.bits))


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

  val cutDelayed = ShiftRegister(cut, depthOfQueue, en = true.B)
  val isPeak = cutDelayed  > threshold

  if (params.numAddPipes == 0 && params.numMulPipes == 0) {
    io.out.bits.peak := isPeak
    if (params.sendCut)
      io.out.bits.cut.get := cutDelayed
    //io.in.ready := ~initialInDone || io.out.ready && ~flushing
    io.out.bits.threshold :=  threshold
    io.out.valid := initialInDone && io.in.fire || flushing
    io.fftBin := cntOut
    io.lastOut := cfarWindow.io.lastOut
    cfarWindow.io.out.ready := io.out.ready
  }
  else {
    val queueData = Module(new Queue((io.out.bits.cloneType), depthOfQueue + 1, flow = true))
    //io.in.ready := ~initialInDone || io.out.ready && ~flushingDelayed

    queueData.io.enq.valid := ShiftRegister(initialInDone && io.in.fire, depthOfQueue, en = true.B) || (flushingDelayed && ShiftRegister(io.out.ready, depthOfQueue, en = true.B))

    if (params.sendCut)
      queueData.io.enq.bits.cut.get := cutDelayed
    queueData.io.enq.bits.threshold := threshold
    queueData.io.enq.bits.peak := isPeak
    queueData.io.deq.ready := io.out.ready
    cfarWindow.io.out.ready := queueData.io.enq.ready


    val queueLast = Module(new Queue(Bool(), depthOfQueue + 1, flow = true))
    queueLast.io.enq.valid := ShiftRegister(initialInDone && io.in.fire, depthOfQueue, en = true.B) || (flushingDelayed && ShiftRegister(io.out.ready, depthOfQueue, en = true.B))
    queueLast.io.enq.bits := lastOut

    queueLast.io.deq.ready := io.out.ready
    io.lastOut := queueLast.io.deq.bits

    if (params.sendCut)
      io.out.bits.cut.get := queueData.io.deq.bits.cut.get
    io.out.bits.peak := queueData.io.deq.bits.peak
    io.out.bits.threshold := queueData.io.deq.bits.threshold
    io.out.valid := queueData.io.deq.valid
    io.fftBin := cntOut
  }
}

object CFARCoreOSApp extends App
{
  // just simple test
  val params: CFARParams[FixedPoint] =  CFARParams(
    protoIn = FixedPoint(16.W, 8.BP),
    protoThreshold = FixedPoint(16.W, 8.BP),
    protoScaler = FixedPoint(16.W, 8.BP),
    leadLaggWindowSize = 32,
    guardWindowSize = 4,
    sendCut = true
  )
  (new ChiselStage).execute(args, Seq(ChiselGeneratorAnnotation(() => new CFARCoreOS(params))))
}
