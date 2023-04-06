package cfar

import chisel3._
import chisel3.experimental._
import chisel3.util._

import dsptools._
import dsptools.numbers._

import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

abstract trait HasShiftRegWithTapsIO extends Module {
  val io: Bundle
}

case class ShiftRegWithTapsParams[T <: Data: Real: BinaryRepresentation](
  protoIn               : T,                               // Data type of the input data
  singleTapDepth        : Int = 16,                        // Depth of the single shift register
  numTaps               : Int = 4,                         // Number of taps -> directly translates to number of outputs
  memType               : String = "SRAM",                 // Define memory type
  runTimeDepth          : Boolean = false
) {
  require(memType == "SRAM" | memType == "REGS")
  require(numTaps > 1)
  require(singleTapDepth > 1)
  requireIsChiselType(protoIn)
}

class ShiftRegWithTapsIO [T <: Data: Real: BinaryRepresentation](params: ShiftRegWithTapsParams[T]) extends Bundle {
  val in = Flipped(Decoupled(params.protoIn))
  val lastIn = Input(Bool())
  val depthRunTime = if (params.runTimeDepth) Some(Input(UInt(log2Up(params.singleTapDepth + 1).W))) else None
  val outs = Vec(params.numTaps, Decoupled(params.protoIn))
  val lastOut = Output(Vec(params.numTaps, Bool()))
}

object ShiftRegWithTapsIO {
  def apply[T <: Data: Real: BinaryRepresentation](params: ShiftRegWithTapsParams[T]): ShiftRegWithTapsIO[T] = new ShiftRegWithTapsIO(params)
}

class ShiftRegWithTaps [T <: Data: Real: BinaryRepresentation](params: ShiftRegWithTapsParams[T]) extends Module with HasShiftRegWithTapsIO {
  val io = IO(ShiftRegWithTapsIO(params))
  // if params.memType == "SRAM"
  // else
}
