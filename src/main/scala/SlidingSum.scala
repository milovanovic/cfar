package cfar

import chisel3._
import chisel3.experimental._
import chisel3.util._

import dsptools._
import dsptools.numbers._

import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

case class SlidingSumParams[T <: Data: Real](
  protoIn               : T,                               // Data type of the input data
  protoOut              : T,                               // Data type of the output data
  depth                 : Int = 16,                        // If guard cells are included then this parameter defines depth of the left and right sliding windows;
                                                           // If guard cells are not enabled then it represents the depth of one unique sliding window
  runTimeDepth          : Boolean = true,                  // Defines whether the run-time configurable depth is enabled or not
  runTimeGuard          : Boolean = true,                  // Defines whether the run-time configurable lenght of guard cells is enabled or not
  guardCells            : Int = 4,                         // Defines the number of guard cells, zero denotes that guard cells are not included at all
  retiming              : Boolean = true,                  // Enables or disables pipe registers inside design

  testWindowSize        : Int = 256,                       // In 2D-CFAR this is a range or Doppler dimension of 2D-CFAR
  runTimeTestWindowSize : Boolean = true                   // Enables run-time configurable test window size
) {
  require(guardCells < depth)
  require(depth > 1)
  requireIsChiselType(protoIn)
  requireIsChiselType(protoOut)
}

class SlidingSum[T <: Data: Real](val params: SlidingSumParams[T]) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(params.protoIn.cloneType))
    val depthRunTime = if (params.runTimeDepth) Some(Input(UInt(log2Up(params.depth + 1).W))) else None
    val guardRunTime = if (params.runTimeDepth) Some(Input(UInt(log2Up(params.guardCells + 1).W))) else None
    val windowRunTime = if (params.runTimeTestWindowSize) Some(Input(UInt(log2Up(params.testWindowSize + 1).W))) else None
    val out = Decoupled(params.protoOut.cloneType)
    val lastIn = Input(Bool())
    val lastOut = Output(Bool())
  })

  if (params.guardCells == 0) {

    val sumT: T = (io.in.bits * log2Ceil(params.depth)).cloneType
    val sumWin = RegInit(t = sumT, init = 0.U.asTypeOf(sumT))

    val cntIn = RegInit(0.U(log2Ceil(params.testWindowSize).W))
    val cntOut = RegInit(0.U(log2Ceil(params.testWindowSize).W))
    val initialInDone = RegInit(false.B)

    val uniqueSlidingWin = Module(new AdjustableShiftRegisterStream(params.protoIn, params.depth))
    uniqueSlidingWin.io.in <> io.in
    uniqueSlidingWin.io.depth := io.depthRunTime.getOrElse(params.depth.U)
    uniqueSlidingWin.io.lastIn := io.lastIn
    uniqueSlidingWin.io.out.ready := io.out.ready // this is a question, maybe it will not be bidirectional connection
    io.lastOut := uniqueSlidingWin.io.lastOut
    val latency = params.depth.U // temporary solution

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
    // regFull -> when regFull then we have a valid output that should be subtracted from the init sum
  }
  else {

  }
}
