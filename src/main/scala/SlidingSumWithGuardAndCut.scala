package cfar

import chisel3._
import chisel3.experimental._
import chisel3.util._

import dsptools._
import dsptools.numbers._

import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

class SlidingSumWithGuardAndCut[T <: Data : Real : BinaryRepresentation](val params: SlidingSumParams[T]) extends Module {

  val io = IO(SlidingSumIO(params))

  val lastCut = RegInit(false.B)
  val flushing = RegInit(false.B)
  val cntIn = RegInit(0.U(log2Ceil(params.testWindowSize).W))
  val cntOut = RegInit(0.U(log2Ceil(params.testWindowSize).W))
  val initialInDone = RegInit(false.B)

  val retiming = if (params.retiming) 1 else 0
  //val depthOfQueue = if (params.retiming) 1 + thresholdPip else thresholdPip
  val latency = io.depthRunTime.getOrElse(params.depth.U) +& (io.guardRunTime.getOrElse(params.guardCells.U) >> 1.U) +& 1.U

  //assert(io.windowRunTime.getOrElse(params.testWindowSize.U) > 2.U * io.depthRunTime.getOrElse(params.depth.U) + 00io.guardRunTime.getOrElse(params.guardCells.U) + 1.U, "Windowing size shall be larger than total number of shifting cells inside sliding sum")
  //assert(io.guardRunTime.getOrElse(params.guardCells.U) > 0.U, "Number of guard cells should be greater than 0")

  val sumT: T = params.protoOut ///(io.in.bits * log2Ceil(params.depth)).cloneType // no overflow
  val sumlagg = RegInit(t = sumT, init = 0.U.asTypeOf(sumT))
  val sumlead = RegInit(t = sumT, init = 0.U.asTypeOf(sumT))

  val slidingWin1 = Module(new AdjustableShiftRegisterStream(params.protoIn, params.depth, false, false, false))
  slidingWin1.io.in <> io.in
  slidingWin1.io.depth := io.depthRunTime.getOrElse(params.depth.U)
  slidingWin1.io.lastIn := io.lastIn

  // not optimal solution but while guardCells size normally is not large then this is ok
  val runTimeGuardCells = 3 to params.guardCells by 2
  val lastOuts = Wire(Vec(runTimeGuardCells.size, Bool()))
  // Remove shift-register with highest depth
  lastOuts.zipWithIndex.map { case (lastOut, index) => lastOut := ShiftRegister(slidingWin1.io.lastOut, (index*2 + 3)/2 + 1, io.out.fire) }

  val lastOut = if (params.runTimeGuard)
                  Mux(io.guardRunTime.get =/= params.guardCells.U, lastOuts(io.guardRunTime.get >> 1.U - 1.U),  ShiftRegister(slidingWin1.io.lastOut, params.guardCells/2 + 1, io.out.fire))
                else
                  ShiftRegister(slidingWin1.io.lastOut, params.guardCells/2 + 1, io.out.fire) // +2 or + 1 - when no adders are included
  val guardCells = Module(new AdjustableShiftRegisterStream(params.protoIn, params.guardCells, true, false, false))
  guardCells.io.in <> slidingWin1.io.out
  guardCells.io.depth := io.guardRunTime.getOrElse(params.guardCells.U)
  guardCells.io.lastIn := slidingWin1.io.lastOut

  val slidingWin2 = Module(new AdjustableShiftRegisterStream(params.protoIn, params.depth, false, false, false))
  slidingWin2.io.in <> guardCells.io.out
  slidingWin2.io.depth := io.depthRunTime.getOrElse(params.depth.U)
  slidingWin2.io.lastIn := guardCells.io.lastOut
  slidingWin2.io.out.ready := io.out.ready

  slidingWin1.io.out.ready := guardCells.io.in.ready
  guardCells.io.out.ready := slidingWin2.io.in.ready

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
  when (cntOut === (io.windowRunTime.getOrElse(params.testWindowSize.U) - 1.U) && io.out.fire || (io.lastOut && io.out.fire)) {
    cntOut := 0.U
  }

  val fftBinOnOutput = cntOut

  when (io.lastIn) {
    flushing := true.B
  }
  val flushingDelayed = ShiftRegisterWithReset(in = flushing,
                                                n = retiming,
                                                resetData = false.B,
                                                reset = io.lastOut,
                                                en = true.B)
  when (io.lastOut && io.out.fire()) {
    lastCut := true.B
    flushing := false.B
    initialInDone := false.B
  }
  when (slidingWin2.io.regEmpty === true.B) {
    lastCut := false.B
  }

  when (io.lastOut) {
    sumlagg := 0.U.asTypeOf(sumT)
  }
  .elsewhen (io.in.fire) {
    when (slidingWin1.io.regFull) {
      when (slidingWin1.io.out.fire) {
        sumlagg := sumlagg + slidingWin1.io.in.bits - slidingWin1.io.out.bits
      }
    }
    .otherwise {
      sumlagg := sumlagg + slidingWin1.io.in.bits
    }
  }
  val condition = WireInit(false.B)
  condition := cntOut > (io.windowRunTime.getOrElse(params.testWindowSize.U) - (io.depthRunTime.getOrElse(params.depth.U)) - (io.guardRunTime.getOrElse(params.guardCells.U) >> 1.U) - 2.U)

  when (condition && slidingWin1.io.out.fire) {
    sumlagg := sumlagg - slidingWin1.io.out.bits
  }

  dontTouch(condition)
  dontTouch(sumlagg)

  when (lastCut) {
    sumlead := 0.U.asTypeOf(sumT)
  }
  .elsewhen (slidingWin2.io.in.fire) {
    when (slidingWin2.io.regFull) {
      when (slidingWin2.io.out.fire) {
        sumlead := sumlead + slidingWin2.io.in.bits - slidingWin2.io.out.bits
      }
    }
    .otherwise {
      sumlead := sumlead + slidingWin2.io.in.bits
    }
  }
  // dontTouch(sumlead)
  // default overflow type is grow
  val finalSum = DspContext.withNumAddPipes(params.numAddPipes) { sumlead context_+ sumlagg }

  val outSum = if (params.retiming == true)
                            RegNext(Mux(!slidingWin2.io.regFull && !slidingWin1.io.regFull,
                              0.U.asTypeOf(finalSum), finalSum))
                          else
                            Mux(!slidingWin2.io.regFull && !slidingWin1.io.regFull,
                              0.U.asTypeOf(finalSum), finalSum)

  val cut = guardCells.io.parallelOut(io.guardRunTime.getOrElse(params.guardCells.U) >> 1.U)
  val cutDelayed = ShiftRegister(cut, params.numAddPipes, en = true.B)

  if (params.numAddPipes == 0) {
    io.out.bits.slidingSum := outSum.asTypeOf(params.protoOut.cloneType)
    io.out.valid := initialInDone && io.in.fire || flushing
    if (params.sendMiddle)
      io.out.bits.middleCell.get := cutDelayed
    //io.in.ready := ~initialInDone || io.out.ready && ~flushing
    io.lastOut := lastOut
  }
  else {
    val queueOut = Module(new Queue((io.out.bits.cloneType), params.numAddPipes + 1, flow = true))
    //io.in.ready := ~initialInDone || io.out.ready && ~flushingDelayed
    queueOut.io.enq.valid := ShiftRegister(initialInDone && io.in.fire, params.numAddPipes, en = true.B) || (flushingDelayed && ShiftRegister(io.out.ready, params.numAddPipes, en = true.B))

    if (params.sendMiddle)
      queueOut.io.enq.bits.middleCell.get := cutDelayed
      io.out.bits.middleCell.get := queueOut.io.deq.bits.middleCell.get

    queueOut.io.enq.bits.slidingSum := outSum.asTypeOf(params.protoOut.cloneType)
    queueOut.io.deq.ready := io.out.ready

    val queueLast = Module(new Queue(Bool(), params.numAddPipes + 1, flow = true))
    queueLast.io.enq.valid := ShiftRegister(initialInDone && io.in.fire, params.numAddPipes, en = true.B) || (flushingDelayed && ShiftRegister(io.out.ready, params.numAddPipes, en = true.B))
    queueLast.io.enq.bits := lastOut

    queueLast.io.deq.ready := io.out.ready
    io.lastOut := queueLast.io.deq.bits

    io.out.bits.slidingSum := queueOut.io.deq.bits.slidingSum
    io.out.valid := queueOut.io.deq.valid
  }
  if (params.sendBin) {
    io.out.bits.bin.get := cntOut
  }
}

object SlidingSumWithGuardAndCutApp extends App
{
  val params: SlidingSumParams[FixedPoint] = SlidingSumParams(
    protoIn = FixedPoint(16.W, 12.BP),
    protoOut = FixedPoint(20.W, 12.BP),
    depth = 16,
    runTimeDepth = true,
    runTimeGuard = true,
    guardCells = 5,
    retiming = false,
    runTimeTestWindowSize = false,
    sendMiddle = true,
    sendBin = true,
    testWindowSize = 256
  )
  (new ChiselStage).execute(args, Seq(ChiselGeneratorAnnotation(() => new SlidingSumWithGuardAndCut(params))))
}

