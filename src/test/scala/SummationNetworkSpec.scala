package cfar

import chisel3._
import chisel3.util._
import chisel3.experimental._
import scala.math._

import chiseltest._
import chiseltest.simulator.{VerilatorFlags}
//import firrtl.AnnotationSeq

import scala.collection._
import scala.util.Random

import dsptools.numbers._
import org.scalatest.flatspec.AnyFlatSpec

trait SummationNetworkTester {

  def compare_data(expected: Double, received: Double, tol: Double) {
    //println(abs(expected - received))
    assert(abs(expected - received) <= tol, "Mismatch!!!")
  }

  def testSumNetwork[T <: Data : Real : BinaryRepresentation](dut: SummationNetwork[T], in: Seq[Double], retiming: Boolean = true, tol: Double = 0.005) = {

    dut.clock.setTimeout(8000)
    val expectedSum = in.sum

    val frac = pow(2, (dut.protoIn match {
      case fp: FixedPoint => fp.binaryPoint.get
      case _ => 0
    }))

    for (index <- 0 until in.size) {
      dut.io.in(index).poke(Real[T].fromDouble(in(index), dut.protoIn))
    }
    if (retiming) {
      dut.clock.step(log2Up(dut.n + 1))
      //dut.io.out.expect(Real[T].fromDouble(expectedSum, dut.protoOut))
      val peekedVal = dut.io.out.peek.litValue
      val peekedValDouble = peekedVal.toDouble/frac
      compare_data(peekedValDouble, expectedSum, tol)
    }
    else {
      val peekedVal = dut.io.out.peek.litValue
      val peekedValDouble = peekedVal.toDouble/frac
      compare_data(peekedValDouble, expectedSum, tol)
      //dut.io.out.expect(Real[T].fromDouble(expectedSum, dut.protoOut))
    }
    dut.clock.step(5)
  }

  def testRunTimeSumNetwork[T <: Data : Real : BinaryRepresentation](dut: SummationNetwork[T], in: Seq[Double], nRunTime: Int = 2, retiming: Boolean = true, tol: Double = 0.5) = {

    dut.clock.setTimeout(8000)
    dut.io.inSize.get.poke(nRunTime.U)
    dut.clock.step(2)
    val expectedSum = in.take(nRunTime).sum

    val frac = pow(2, (dut.protoIn match {
      case fp: FixedPoint => fp.binaryPoint.get
      case _ => 0
    }))

    for (index <- 0 until in.size) {
      if (index < nRunTime) {
        dut.io.in(index).poke(Real[T].fromDouble(in(index), dut.protoIn))
      }
      else {
        dut.io.in(index).poke(Real[T].fromDouble(0.0, dut.protoIn))
      }
    }
    if (retiming) {
      //dut.clock.step(log2Up(nRunTime + 1))
      //println(log2Up(nRunTime) + 1)
      dut.clock.step(log2Up(nRunTime) + 1)
      //dut.io.out.expect(Real[T].fromDouble(expectedSum, dut.protoOut))
      val peekedVal = dut.io.out.peek.litValue
      val peekedValDouble = peekedVal.toDouble/frac
      compare_data(peekedValDouble, expectedSum, tol)
    }
    else {
      val peekedVal = dut.io.out.peek.litValue
      val peekedValDouble = peekedVal.toDouble/frac
      compare_data(peekedValDouble, expectedSum, tol)
      //dut.io.out.expect(Real[T].fromDouble(expectedSum, dut.protoOut))
    }
    dut.clock.step(5)
  }
}

class SumNetworkSpec extends AnyFlatSpec with ChiselScalatestTester with SummationNetworkTester {

  var next_n = 8
  Random.setSeed(11110L)

  // test UInt data type
  for (retiming <- Seq(false, true)) {
    for (n <- Seq(4, 5, 8, 9, 32)) {
      for (runTime <- Seq(false)) {
        it should f"work for UInt, n equal to $n,  retiming = $retiming, runTime = $runTime" ignore {
          if (isPow2(n) || isPow2(n-1)) {
            next_n = n
          }
          else {
            next_n = pow(2, log2Ceil(n)).toInt
          }
          val inputProto = UInt(16.W)
          val outputProto = UInt((16 + log2Up(n)).W)
          val in = Seq.fill(n)(Random.nextInt(1<<(inputProto.getWidth)).toDouble)
          test(new SummationNetwork(inputProto,
                                    outputProto,
                                    next_n,
                                    runTime,
                                    retiming)).withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut => testSumNetwork(dut, in, retiming, tol = 0) }
        }
      }
    }
  }

  // test FixedPoint data type
  for (retiming <- Seq(false, true)) {
    for (n <- Seq(4, 5, 8, 9, 32)) {
      for (runTime <- Seq(false)) {
        it should f"work for FixedPoint, n equal to $n,  retiming = $retiming, runTime = $runTime" ignore {
          if (isPow2(n) || isPow2(n-1)) {
            next_n = n
          }
          else {
            next_n = pow(2, log2Ceil(n)).toInt
          }
          val inputProto = FixedPoint(16.W, 8.BP)
          val outputProto = FixedPoint((16 + log2Up(n)).W, 8.BP)
          val in: Seq[Double] = Seq.fill(n)((Random.nextDouble()*2-1) * ((1<<inputProto.getWidth - inputProto.binaryPoint.get-1)))

          test(new SummationNetwork(inputProto,
                                    outputProto,
                                    next_n,
                                    runTime,
                                    retiming)).withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut => testSumNetwork(dut, in, retiming, tol = 0.5) }
        }
      }
    }
  }

  // test SInt data type
  for (retiming <- Seq(true, false)) {
    for (n <- Seq(4, 5, 8)) {
      for (runTime <- Seq(false, true)) {
        if (isPow2(n) || isPow2(n-1)) {
          next_n = n
        }
        else {
          next_n = pow(2, log2Ceil(n)).toInt
        }
        val inputProto = SInt(16.W)
        val outputProto = SInt((16 + log2Up(n)).W)
        val in = Seq.fill(n)((Random.nextInt((1<<(inputProto.getWidth-1))*2) - (1<<(inputProto.getWidth-1))).toDouble)

        if (runTime) {
          val indices = 2 to n by 1
          for (r <- indices) {
            it should f"work for SInt, n equal to $n,  retiming = $retiming, runTime = true and runtime n value equal to $r" in {
              test(new SummationNetwork(inputProto,
                                        outputProto,
                                        next_n,
                                        runTime,
                                        retiming)).withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut => testRunTimeSumNetwork(dut, in, r, retiming, tol = 0) }
            }
          }
        }
        else {
            it should f"work for SInt, n equal to $n,  retiming = $retiming, runTime = false" in {
              test(new SummationNetwork(inputProto,
                                        outputProto,
                                        next_n,
                                        runTime,
                                        retiming)).withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut => testSumNetwork(dut, in, retiming, tol = 0) }
          }
        }
      }
    }
  }
}


