package cfar

import chisel3._
import chisel3.experimental._
import chisel3.util._

import dsptools._
import dsptools.numbers._

import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

abstract trait HasSimpleSplitterIO extends Module {
  val io: Bundle
}


class SimpleSplitterIO [T <: Data: Real](proto: T) extends Bundle {
  val in = Flipped(Decoupled(proto))
  val lastIn = Input(Bool())
  val out1 = Decoupled(proto)
  val out2 = Decoupled(proto)
  val lastOut1 = Output(Bool())
  val lastOut2 = Output(Bool())
}

object SimpleSplitterIO {
  def apply[T <: Data: Real](proto: T): SimpleSplitterIO[T] = new SimpleSplitterIO(proto)
}

// : BinaryRepresentation
class SimpleSplitter [T <: Data: Real](proto: T) extends Module with HasSimpleSplitterIO {
  val io = IO(SimpleSplitterIO(proto))

  io.in.ready := io.out1.ready && io.out2.ready
  io.out1.valid := io.in.valid
  io.out2.valid := io.in.valid && io.out1.ready
  io.out1.bits := io.in.bits
  io.out2.bits := io.in.bits

  io.lastOut1 := io.lastIn && io.out1.fire
  io.lastOut2 := io.lastIn && io.out2.fire
}

object SimpleSplitterApp extends App
{
  (new ChiselStage).execute(args, Seq(ChiselGeneratorAnnotation(() => new SimpleSplitter(FixedPoint(16.W, 14.BP)))))
}
