package cfar

import chisel3._
import chisel3.experimental._
import chisel3.util._

import dsptools._
import dsptools.numbers._

import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

object FindMin
{
  /** Find minimum inside input vector data
    *
    * @param in1 input1 data sequence (input to the MinimumCircuit)
    * @param in2 input2 data sequence (min seq from the upper layer)
    * @param n size of the input sequence
    * @param size if run time is not active have the same value as parameter n
    *
    * @example {{{
    * val findMin = FindMin(in1, in2, 16, 16.U)
    * }}}
    */
  def apply[T <: Data: Real](in1: Seq[T], in2: Seq[T], n: Int, size: Option[UInt]): T = {
    require(n > 0, "Input vector size should be positive")
    if (n != 1) {
      val sel_n = n.U === size.getOrElse(0.U)
      // if run time is off provides that sel signals are always false
      val input_to_min = in1.zip(in2).map { case (a,b) => Mux(sel_n, a, b) }
      val mins = input_to_min.grouped(2).map(pair => Mux(pair(0) < pair(1), pair(0), pair(1)))
      apply(in1.take(n/2), mins.toSeq, n/2, size)
    } else {
      in2.head
    }
  }
}

class MinimumCircuit[T <: Data: Real](val protoIn: T, val n: Int = 16, val runTime: Boolean = true, val retiming: Boolean = false) extends Module {
  require(n > 0 && isPow2(n), "Size of the input vector must be a power of 2 and it should be positive")

  val io = IO(new Bundle {
    val in = Input(Vec(n, protoIn))
    val inSize = if (runTime) Some(Input(UInt(log2Up(n+1).W))) else None
    val out = Output(protoIn)
  })
  
  val size = io.inSize.getOrElse(n)
  
  if (n != 1) {
    // first layer should split input vector to pairs, compare and pass min to FindMin object
    val mins = io.in.grouped(2).toSeq.map(pair => Mux(pair(0) < pair(1), pair(0), pair(1)))
    val minFinal = if (retiming) RegNext(FindMin(io.in.take(n/2), mins, n/2, io.inSize)) else FindMin(io.in.take(n/2), mins, n/2, io.inSize)
    if (runTime == true) {
      when (io.inSize.get === 1.U) {
        if (retiming)
          io.out := RegNext(io.in(0))
        else
          io.out := io.in(0)
      }
      .otherwise {
        io.out := minFinal
      }
    }
    else {
      io.out := minFinal
    }
  }
  else {
    if (retiming)
      io.out := RegNext(io.in(0))
    else
      io.out := io.in(0)
  }
}


object MinimumCircuitApp extends App
{
  //chisel3.Driver.execute(args,()=>new MinimumCircuit(FixedPoint(16.W, 14.BP))) // replace with ChiselStage
  chisel3.Driver.execute(args,()=>new MinimumCircuit(FixedPoint(16.W, 14.BP), runTime = false)) // replace with ChiselStage
}






