package cfar

import chisel3._
import chisel3.experimental._
import chisel3.util._

import dsptools._
import dsptools.numbers._

import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}




object FindSum
{
  /** Find minimum inside input vector data
    *
    * @param in1 input1 data sequence (input to the SummationNetwork)
    * @param in2 input2 data sequence (sum seq from the upper layer)
    * @param n size of the input sequence
    * @param size if run time is not active have the same value as parameter n
    *
    * @example {{{
    * val findMin = FindMin(in1, in2, 16, 16.U)
    * }}}
    */
  // n needs to always be power of 2 here!
  def apply[T <: Data: Real](in1: Seq[T], in2: Seq[T], n: Int, size: Option[UInt], retiming: Boolean = false): T = {
    require(n > 0, "Input vector size should be positive")
    if (n != 1) {
      val sel_n = (n/2 + 1).U > size.getOrElse(n.U)
      // if run time is off provides that sel signals are always false
      val input_to_sums = in1.zip(in2).map { case (a,b) => Mux(sel_n, a, b) }
      val sums = DspContext.alter(
                  DspContext.current.copy(overflowType = Grow, binaryPointGrowth = 2)) {
                    if (retiming == true) input_to_sums.grouped(2).map(pair => RegNext(pair(0) context_+ pair(1))) else input_to_sums.grouped(2).map(pair => pair(0) context_+ pair(1))
                  }
      apply(in1.take(n/2), sums.toSeq, n/2, size, retiming)
    } else {
      in2.head
    }
  }
}

// binaryGrowth logic should be considered, but I believe that it is not necessary to use that, that is
// https://stackoverflow.com/questions/53095845/vector-of-regenable
// DspContext should be defined, retiming can be defined in the form of vectors of enable signals
// +& does not work for parameter T, define grow logic inside DspContext!
// maybe add enable signal and valid signal as well inside input interface
class SummationNetwork[T <: Data: Real](val protoIn: T, val protoOut: T, val n: Int = 16, val runTime: Boolean = true, val retiming: Boolean = false) extends Module {
  require(n > 0, "Size of the input vector should be positive")
  val io = IO(new Bundle {
    val in = Input(Vec(n, protoIn)) // it is very important that data that are not used
    val inSize = if (runTime) Some(Input(UInt(log2Up(n+1).W))) else None
    val out = Output(protoOut)
  })

  val size = io.inSize.getOrElse(n.U)

  if (n != 1) {
    if (n % 2 == 0) {
      val partial_sums = if (retiming == true) io.in.grouped(2).toSeq.map(pair => DspContext.alter(DspContext.current.copy(overflowType = Grow, binaryPointGrowth = 2)) { RegNext(pair(0) context_+ pair(1)) } ) else io.in.grouped(2).toSeq.map(pair =>  DspContext.alter(DspContext.current.copy(overflowType = Grow, binaryPointGrowth = 2)) { pair(0) context_+ pair(1) } )
      val sumFinal = if (retiming) RegNext(FindSum(io.in.take(n/2), partial_sums, n/2, io.inSize, retiming)) else FindSum(io.in.take(n/2), partial_sums, n/2, io.inSize, retiming)

      when (size % 2.U === 0.U) {
        if (runTime == true) {
          when (io.inSize.get === 1.U) {
            if (retiming)
              io.out := RegNext(io.in(0))
            else
              io.out := io.in(0)
          }
          .otherwise {
            io.out := sumFinal
          }
        }
        else {
          io.out := sumFinal
        }
      }
      .otherwise {
        if (runTime == true) {
          when (io.inSize.get === 1.U) {
            if (retiming)
              io.out := RegNext(io.in(0))
            else
              io.out := io.in(0)
          }
          .otherwise {
            io.out :=  sumFinal
          }
        }
        else {
          io.out :=  DspContext.alter(DspContext.current.copy(overflowType = Grow)) { sumFinal + io.in(n - 1) }
        }
      }
    }
    else {
      // RegNext should be RegEnable because of clock gating logic insertion
      val partial_sums = if (retiming == true) io.in.take(n-1).grouped(2).toSeq.map(pair => DspContext.alter(DspContext.current.copy(overflowType = Grow, binaryPointGrowth = 2)) { RegNext(pair(0) context_+ pair(1)) }) else io.in.take(n-1).grouped(2).toSeq.map(pair => DspContext.alter(DspContext.current.copy(overflowType = Grow, binaryPointGrowth = 2)) { pair(0) context_+ pair(1) })
      //println(partial_sums(0).getWidth)

      val sumFinal = if (retiming) RegNext(FindSum(io.in.take(n/2), partial_sums, n/2, io.inSize)) else FindSum(io.in.take(n/2), partial_sums, n/2, io.inSize)

      when (size % 2.U === 0.U) {
        if (runTime == true) {
          when (io.inSize.get === 1.U) {
            if (retiming)
              io.out := RegNext(io.in(0))
            else
              io.out := io.in(0)
          }
          .otherwise {
            io.out := sumFinal
          }
        }
        else {
          io.out := sumFinal
        }
      }
      .otherwise {
        if (runTime == true) {
          when (io.inSize.get === 1.U) {
            if (retiming)
              io.out := RegNext(io.in(0))
            else
              io.out := io.in(0)
          }
          .otherwise {
            when (io.inSize.get === n.U) {
              io.out := DspContext.alter(DspContext.current.copy(overflowType = Grow, binaryPointGrowth = 2)) { sumFinal context_+ io.in(io.inSize.get - 1.U) }
            }
            .otherwise {
              io.out := sumFinal
            }
          }
        }
        else {
          io.out := DspContext.alter(DspContext.current.copy(overflowType = Grow, binaryPointGrowth = 2)) { sumFinal context_+ io.in(n - 1) }
        }
      }
    }
  }
  else {
    if (retiming)
      io.out := RegNext(io.in(0))
    else
      io.out := io.in(0)
  }
}


object SummationNetworkApp extends App
{
  (new ChiselStage).execute(args, Seq(ChiselGeneratorAnnotation(() => new SummationNetwork(FixedPoint(16.W, 14.BP), FixedPoint(20.W, 14.BP), runTime = true, retiming = false))))
}
