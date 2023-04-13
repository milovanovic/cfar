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
  runTimeTaps           : Boolean = false,                 // Run-time configurable number of taps - this is important to have in order to reduce power
  memType               : String = "SRAM",                 // Define memory type
  runTimeDepth          : Boolean = false                  // Enable or disable run time configurable singleTapDepth
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
  val tapsRunTime = if (params.runTimeTaps) Some(Input(UInt(log2Up(params.numTaps + 1).W))) else None
  val outs = Vec(params.numTaps, Decoupled(params.protoIn))
  val lastOut = Output(Vec(params.numTaps, Bool()))
}

object ShiftRegWithTapsIO {
  def apply[T <: Data: Real: BinaryRepresentation](params: ShiftRegWithTapsParams[T]): ShiftRegWithTapsIO[T] = new ShiftRegWithTapsIO(params)
}

class ShiftRegWithTaps [T <: Data: Real: BinaryRepresentation](params: ShiftRegWithTapsParams[T]) extends Module with HasShiftRegWithTapsIO {
  val io = IO(ShiftRegWithTapsIO(params))

  val activeTaps = Wire(Vec(params.numTaps, Bool()))
  val numTaps = io.tapsRunTime.getOrElse(params.numTaps.U)

  // this will influence value of valid signals
  activeTaps.zipWithIndex.map { case (active, index) => {
      active := Mux(numTaps >= index.U, true.B, false.B) // <> is not ok to have
    }
  }

  if (params.memType == "SRAM") {
    val shiftRegsWithTaps = Seq.fill(params.numTaps)(Module(new ShiftRegisterMemStream(params.protoIn, params.singleTapDepth, enInitStore = true)))

    shiftRegsWithTaps(0).io.in <> io.in
    shiftRegsWithTaps(0).io.lastIn := io.lastIn
    shiftRegsWithTaps.foreach { c => c.io.depth := io.depthRunTime.getOrElse(params.singleTapDepth.U) }

    io.outs(params.numTaps-1) <> shiftRegsWithTaps(params.numTaps-1).io.out
    io.lastOut(params.numTaps-1) := shiftRegsWithTaps(params.numTaps-1).io.lastOut

    shiftRegsWithTaps.tail.map(_.io).zipWithIndex.foldLeft(shiftRegsWithTaps(0).io) {
      case (shiftReg1_io, (shiftReg2_io, index)) => {
        val splitter = Module(new SimpleSplitter(params.protoIn))
        splitter.io.in <> shiftReg1_io.out
        splitter.io.lastIn := shiftReg1_io.lastOut
        shiftReg2_io.in <> splitter.io.out2
        shiftReg2_io.in.valid := activeTaps(index) && splitter.io.out2.valid
        io.outs(index) <> splitter.io.out1
        io.lastOut(index) := splitter.io.lastOut1
        shiftReg2_io.lastIn := splitter.io.lastOut2
        shiftReg2_io
      }
    }
  }
  else {
    val shiftRegsWithTaps = Seq.fill(params.numTaps)(Module(new AdjustableShiftRegisterStream(params.protoIn, params.singleTapDepth, enInitStore = true)))
    shiftRegsWithTaps(0).io.in <> io.in
    shiftRegsWithTaps(0).io.lastIn := io.lastIn
    shiftRegsWithTaps.foreach { c => c.io.depth := io.depthRunTime.getOrElse(params.singleTapDepth.U) }

    io.outs(params.numTaps-1) <> shiftRegsWithTaps(params.numTaps-1).io.out
    io.lastOut(params.numTaps-1) := shiftRegsWithTaps(params.numTaps-1).io.lastOut
    shiftRegsWithTaps.tail.map(_.io).zipWithIndex.foldLeft(shiftRegsWithTaps(0).io) {
      case (shiftReg1_io, (shiftReg2_io, index)) => {
        val splitter = Module(new SimpleSplitter(params.protoIn))
        splitter.io.in <> shiftReg1_io.out
        splitter.io.lastIn := shiftReg1_io.lastOut
        shiftReg2_io.in <> splitter.io.out1
        shiftReg2_io.in.valid := activeTaps(index) && splitter.io.out2.valid
        io.outs(index) <> splitter.io.out2
        io.lastOut(index) := splitter.io.lastOut1
        shiftReg2_io.lastIn := splitter.io.lastOut2
        shiftReg2_io
      }
    }
  }
}

object ShiftRegWithTapsApp extends App
{
  val params: ShiftRegWithTapsParams[FixedPoint] = ShiftRegWithTapsParams(
    protoIn = FixedPoint(16.W, 10.BP),   // Data type of the input data
    singleTapDepth = 16,                 // Depth of the single shift register
    numTaps = 4,                         // Number of taps -> directly translates to number of outputs
    runTimeTaps = true,                 // Run-time configurable number of taps - this is important to have in order to reduce power
    memType = "SRAM",
    runTimeDepth = true)

  (new ChiselStage).execute(args, Seq(ChiselGeneratorAnnotation(() => new ShiftRegWithTaps(params))))
}
