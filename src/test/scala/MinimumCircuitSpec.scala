package cfar

import chisel3._
import chisel3.experimental._
import chisel3.util.log2Up

import dsptools._
import dsptools.numbers._

import org.scalatest.{FlatSpec, Matchers}
import scala.util.Random
import scala.math.pow

class MinimumCircuitTester[T <: Data: Real](dut: MinimumCircuit[T], tol: Int) extends DspTester(dut) {
  
  if (dut.runTime) {
    val pow2Coll = (1 to log2Up(dut.n)).map(p => pow(2, p).toInt)
    var testSignal = dut.protoIn match {
      case f: FixedPoint => Seq.fill(dut.n)((Random.nextDouble() * 2 - 1) * ((1 << dut.protoIn.getWidth - f.binaryPoint.get - 1)))
      //case f: FixedPoint => (0 until maxDepth * 2).map(c => c.toDouble) // Seq.fill(maxDepth * 2)((Random.nextDouble() * 2 - 1) * ((1 << proto.getWidth - f.binaryPoint.get - 1)))
      case s: SInt => Seq.fill(dut.n)((Random.nextInt((1 << (dut.protoIn.getWidth - 1)) * 2) - (1 << (dut.protoIn.getWidth - 1))).toDouble)
      case u: UInt => Seq.fill(dut.n)(Random.nextInt(1 << (dut.protoIn.getWidth)).toDouble)
      case d: DspReal => Seq.fill(dut.n)((Random.nextInt(Double.MaxValue.toInt) - Double.MaxValue.toInt).toDouble)
    }
    for (size <- pow2Coll) {
      var min = testSignal.take(size).min
      // define all inputs even though some of them are not considered for min calculation
      updatableDspVerbose.withValue(false) {
        testSignal.zipWithIndex.foreach { case (sample, index) => poke(dut.io.in(index), sample)}
        poke(dut.io.inSize.get, size)
        dut.protoIn match {
          case dspR: DspReal => realTolDecPts.withValue(tol) { expect(dut.io.out, min) }
          case _ => fixTolLSBs.withValue(tol) { expect(dut.io.out, min) }
        }
        step(2)
      }
    }
  }
  else {
    var testSignal = dut.protoIn match {
      case f: FixedPoint => Seq.fill(dut.n)((Random.nextDouble() * 2 - 1) * ((1 << dut.protoIn.getWidth - f.binaryPoint.get - 1)))
      //case f: FixedPoint => (0 until maxDepth * 2).map(c => c.toDouble) // Seq.fill(maxDepth * 2)((Random.nextDouble() * 2 - 1) * ((1 << proto.getWidth - f.binaryPoint.get - 1)))
      case s: SInt => Seq.fill(dut.n)((Random.nextInt((1 << (dut.protoIn.getWidth - 1)) * 2) - (1 << (dut.protoIn.getWidth - 1))).toDouble)
      case u: UInt => Seq.fill(dut.n)(Random.nextInt(1 << (dut.protoIn.getWidth)).toDouble)
      case d: DspReal => Seq.fill(dut.n)((Random.nextInt(Double.MaxValue.toInt) - Double.MaxValue.toInt).toDouble)
    }
    var min = testSignal.min
    updatableDspVerbose.withValue(false) {
      testSignal.zipWithIndex.foreach { case (sample, index) => poke(dut.io.in(index), sample)}
      dut.protoIn match {
        case dspR: DspReal => realTolDecPts.withValue(tol) { expect(dut.io.out, min) }
        case _ => fixTolLSBs.withValue(tol) { expect(dut.io.out, min) }
      }
      step(4)
    }
  }
}

object MinimumCircuitTester {
  def apply[T <: Data : Real](protoIn: T, n: Int = 16 , runTime: Boolean = false, tol: Int = 1): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator"),
      () => new MinimumCircuit(protoIn, n, runTime)) { c =>
      new MinimumCircuitTester(c, tol)
    }
  }
}

class MinimumCircuitSpec extends FlatSpec with Matchers {
  
  // FixedPoint
  for (size <- Seq(2, 4, 8, 16, 32)) {
    it should s"work with input size vector equal to $size and FixedPoint data input" in {
      MinimumCircuitTester(FixedPoint(16.W, 5.BP), n = size, tol = 2) should be (true)
    }
  }
  
  for (size <- Seq(2, 8, 16, 32, 64)) {
    it should s"work with input size vector equal to $size, enabled run time configurable input size and FixedPoint data input" in {
      MinimumCircuitTester(FixedPoint(16.W, 5.BP), n = size, runTime = true, tol = 2) should be (true)
    }
  }
  
  // SInt
  for (size <- Seq(2, 8, 16, 32, 64)) {
    it should s"work with input size vector equal to $size, enabled run time configurable input size and SInt data input" in {
      MinimumCircuitTester(SInt(16.W), n = size, runTime = true, tol = 0) should be (true)
    }
  }
  // UInt
  for (size <- Seq(2, 8, 16, 32, 64)) {
    it should s"work with input size vector equal to $size, enabled run time configurable input size and UInt data input" in {
      MinimumCircuitTester(UInt(16.W), n = size, runTime = true, tol = 0) should be (true)
    }
  }
  // DspReal
  for (size <- Seq(2, 8, 16, 32, 64)) {
    it should s"work with input size vector equal to $size, enabled run time configurable input size and DspReal data input" in {
      MinimumCircuitTester(DspReal(), n = size, runTime = true, tol = 12) should be (true)
    }
  }
}
