package cfar

import chisel3._
import chisel3.experimental._
import chisel3.util._

import dsptools._
import dsptools.numbers._

import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

abstract trait HasSlidingSumIO extends Module {
  val io: Bundle
}

class SlidingSumOutFields [T <: Data: Real: BinaryRepresentation] (protoIn: T, protoOut: T, sendMiddle: Boolean, sendBin: Boolean, windowSize: Int, sendNeighbs: Boolean) extends Bundle {
  val bin            = if (sendBin) Some(Output(UInt(log2Ceil(windowSize).W))) else None
  val middleCell     = if (sendMiddle) Some(Output(protoIn)) else None // Middle cell can be neighbour cell as well in vertical direction
  val leftNeighbour  = if (sendNeighbs) Some(Output(protoIn)) else None
  val rightNeighbour = if (sendNeighbs) Some(Output(protoIn)) else None
  val slidingSum     = Output(protoOut)
}

// think about adding  more than one method for edge cases!
case class SlidingSumParams[T <: Data: Real: BinaryRepresentation](
  protoIn               : T,                               // Data type of the input data
  protoOut              : T,                               // Data type of the output data
  depth                 : Int = 16,                        // If guard cells are included then this parameter defines depth of the left and right sliding windows;
                                                           // If guard cells are not enabled then it represents the depth of one unique sliding window
  runTimeDepth          : Boolean = true,                  // Defines whether the run-time configurable depth is enabled or not
  runTimeGuard          : Boolean = true,                  // Defines whether the run-time configurable lenght of guard cells is enabled or not
  guardCells            : Int = 4,                         // Defines the number of guard cells, zero denotes that guard cells are not included at all
  retiming              : Boolean = true,                  // Enables or disables pipe registers inside design

  testWindowSize        : Int = 256,                       // In 2D-CFAR this is a range or Doppler dimension of 2D-CFAR
  runTimeTestWindowSize : Boolean = true,                  // Enables run-time configurable test window size
  sendMiddle            : Boolean = false,                 // For 2D-CFAR this is cell under test
  sendNeighbs           : Boolean = false,                 // Can be useful for peakGrouping logic
  sendBin               : Boolean = false,                 // Current FFT bin
  numAddPipes           : Int = 0                          // Number of pipes added after plus operator
) {
  //require(guardCells <= depth)
  require(depth > 1)
  requireIsChiselType(protoIn)
  requireIsChiselType(protoOut)
}

class SlidingSumIO [T <: Data: Real: BinaryRepresentation](params: SlidingSumParams[T]) extends Bundle {
  val in = Flipped(Decoupled(params.protoIn))
  val lastIn = Input(Bool())
  val depthRunTime = if (params.runTimeDepth) Some(Input(UInt(log2Up(params.depth + 1).W))) else None
  val guardRunTime = if (params.runTimeGuard) Some(Input(UInt(log2Up(params.guardCells + 1).W))) else None
  val windowRunTime = if (params.runTimeTestWindowSize) Some(Input(UInt(log2Up(params.testWindowSize + 1).W))) else None
  val out = Decoupled(new SlidingSumOutFields(params.protoIn, params.protoOut, params.sendMiddle, params.sendBin, params.testWindowSize, params.sendNeighbs)) //Decoupled(params.protoOut.cloneType)
  val lastOut = Output(Bool())
}

object SlidingSumIO {
  def apply[T <: Data: Real: BinaryRepresentation](params: SlidingSumParams[T]): SlidingSumIO[T] = new SlidingSumIO(params)
}


class SlidingSum[T <: Data: Real: BinaryRepresentation](val params: SlidingSumParams[T]) extends Module with HasSlidingSumIO {
  val io = IO(SlidingSumIO(params))

  if (params.guardCells == 0) {
    // this sliding sum needs to follow other sliding sums, result should be available in the same moment as it is for other sliding sums
    // this sliding sum can be used later for OSCA CFAR that includes linear sorter as the solution
    // think about moving this code to separate modul.

    val sumT: T = params.protoOut //(io.in.bits * log2Ceil(params.depth + 1)).cloneType
    val sumWin = RegInit(t = sumT, init = 0.U.asTypeOf(sumT))

    val cntIn = RegInit(0.U(log2Ceil(params.testWindowSize).W))
    val cntOut = RegInit(0.U(log2Ceil(params.testWindowSize).W))
    val initialInDone = RegInit(false.B)
    val flushing = RegInit(false.B)
    val condition = WireInit(false.B)

    val uniqueSlidingWin = Module(new AdjustableShiftRegisterStream(params.protoIn, params.depth, false, false, false))
    uniqueSlidingWin.io.in <> io.in
    uniqueSlidingWin.io.depth := io.depthRunTime.getOrElse(params.depth.U)
    uniqueSlidingWin.io.lastIn := io.lastIn
    uniqueSlidingWin.io.out.ready := io.out.ready
    io.lastOut := uniqueSlidingWin.io.lastOut
    val latency = io.depthRunTime.getOrElse(params.depth.U) >> 1 // half of the window
    io.out.valid := initialInDone && io.in.fire || flushing
    io.out.bits.slidingSum := sumWin
    //io.in.ready := ~initialInDone || io.out.ready && ~flushing

    //  val latency = io.windowCells +& io.guardCells +& 1.U //+& thresholdPip.U // + retimingInt
    when (io.lastIn) {
      flushing := true.B
    }

    when (io.in.fire) {
      cntIn := cntIn + 1.U
    }
    when (cntIn === latency && io.in.fire) {
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

    when (io.lastOut && io.out.fire) {
      flushing := false.B
      initialInDone := false.B
    }

    condition := cntOut > (io.windowRunTime.getOrElse(params.testWindowSize.U) - (io.depthRunTime.getOrElse(params.depth.U) >> 1) - 2.U)

    when (io.lastOut) {
      sumWin := 0.U.asTypeOf(sumT)
    }
    .elsewhen (io.in.fire) {
      when (uniqueSlidingWin.io.regFull) {
        when (uniqueSlidingWin.io.out.fire) {
          sumWin := sumWin + uniqueSlidingWin.io.in.bits - uniqueSlidingWin.io.out.bits
        }
      }
      .otherwise {
        sumWin := sumWin + uniqueSlidingWin.io.in.bits
      }
    }

    when (condition && io.out.fire) {
      sumWin := sumWin - uniqueSlidingWin.io.out.bits
    }
  }
  else {
    val slidingSum = Module(new SlidingSumWithGuardAndCut(params))
    slidingSum.io <> io
  }
}

object SlidingSumApp extends App
{
  val params: SlidingSumParams[FixedPoint] = SlidingSumParams(
    protoIn = FixedPoint(16.W, 12.BP),
    protoOut = FixedPoint(20.W, 12.BP),
    depth = 16,
    runTimeDepth = true,
    runTimeGuard = false,
    guardCells = 0,
    retiming = false,
    runTimeTestWindowSize = false,
    testWindowSize = 256
  )
  (new ChiselStage).execute(args, Seq(ChiselGeneratorAnnotation(() => new SlidingSum(params))))
}


