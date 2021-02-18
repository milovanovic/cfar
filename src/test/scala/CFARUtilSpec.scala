package cfar

import chisel3._
import chisel3.experimental._

import dsptools._
import dsptools.numbers._

import org.scalatest.{FlatSpec, Matchers}
import scala.util.Random


class AdjShiftRegStreamTester[T <: Data](dut: AdjustableShiftRegisterStream[T], in: Seq[Double], tol: Int) extends DspTester(dut) {
  
  var cntValidOut = 0
  // val currentDepth = dut.maxDepth
  
  for (currentDepth <- 0 to dut.maxDepth) { // try to run currentDepth to be equal to 0
    poke(dut.io.in.valid, 0)
    poke(dut.io.out.ready, 0)
    poke(dut.io.depth, currentDepth)
    step(2)
    
    poke(dut.io.out.ready, 1)
    for (i <- 0 until in.size) {
      poke(dut.io.in.valid, 0)
      val delay = 3 //Random.nextInt(5)
      step(delay)
      poke(dut.io.in.valid, 1)
      poke(dut.io.in.bits, in(i))
      if (i == (in.size - 1))
        poke(dut.io.lastIn, 1)
      if (peek(dut.io.out.valid) == true) {
        dut.proto match {
          case dspR: DspReal => realTolDecPts.withValue(tol) { expect(dut.io.out.bits,  in(cntValidOut)) }
          case _ =>  fixTolLSBs.withValue(tol) { expect(dut.io.out.bits, in(cntValidOut)) }
        }
        cntValidOut += 1
      }
      step(1)
    }
    poke(dut.io.lastIn, 0)
    poke(dut.io.in.valid, 0)
    poke(dut.io.out.ready, 0) // it should not shift if output is not ready to accept data
    step(10)
    poke(dut.io.out.ready, 1)
    
    while (cntValidOut < in.size) {
      if (peek(dut.io.out.valid) == true) {
        dut.proto match {
          case dspR: DspReal => realTolDecPts.withValue(tol) { expect(dut.io.out.bits,  in(cntValidOut)) }
          case _ =>  fixTolLSBs.withValue(tol) { expect(dut.io.out.bits, in(cntValidOut)) }
        }
        if (cntValidOut == in.size - 1)
          expect(dut.io.lastOut, 1)
        cntValidOut += 1
      }
      step(1)
    }
    cntValidOut = 0
    step(2)
  }
}

object AdjShiftRegStreamTester {
  def apply[T <: Data : Ring](proto: T, maxDepth: Int, tol: Int): Boolean = {
    val testSignal = proto match {
     // case f: FixedPoint => Seq.fill(maxDepth * 2)((Random.nextDouble() * 2 - 1) * ((1 << proto.getWidth - f.binaryPoint.get - 1)))
     case f: FixedPoint => (0 until maxDepth * 2).map(c => c.toDouble) // Seq.fill(maxDepth * 2)((Random.nextDouble() * 2 - 1) * ((1 << proto.getWidth - f.binaryPoint.get - 1)))
     case s: SInt => Seq.fill(maxDepth * 2)((Random.nextInt((1 << (proto.getWidth - 1)) * 2) - (1 << (proto.getWidth - 1))).toDouble)
      case u: UInt => Seq.fill(maxDepth * 2)(Random.nextInt(1 << (proto.getWidth)).toDouble)
      case d: DspReal => Seq.fill(maxDepth * 2)((Random.nextInt(Double.MaxValue.toInt) - Double.MaxValue.toInt).toDouble)
    }
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator"),
      () => new AdjustableShiftRegisterStream(proto, maxDepth)) { c =>
      new AdjShiftRegStreamTester(c, testSignal, tol)
    }
  }
}

class CellUnderTestTester[T <: Data](dut: CellUnderTest[T], in: Seq[Double], tol: Int) extends DspTester(dut) {
  
  var cntValidOut = 0
  poke(dut.io.in.valid, 0)
  poke(dut.io.out.ready, 0)
  step(2)
  
  poke(dut.io.out.ready, 1)
  for (i <- 0 until in.size) {
    poke(dut.io.in.valid, 0)
    val delay = Random.nextInt(5)
    step(delay)
    poke(dut.io.in.valid, 1)
    poke(dut.io.in.bits, in(i))
    if (i == (in.size - 1))
      poke(dut.io.lastIn, 1)
    if (peek(dut.io.out.valid) == true) {
      dut.proto match {
        case dspR: DspReal => realTolDecPts.withValue(tol) { expect(dut.io.out.bits,  in(cntValidOut)) }
        case _ =>  fixTolLSBs.withValue(tol) { expect(dut.io.out.bits, in(cntValidOut)) }
      }
      cntValidOut += 1
    }
    step(1)
  }
  poke(dut.io.lastIn, 0)
  poke(dut.io.in.valid, 0)
  poke(dut.io.out.ready, 0) // it should not shift if output is not ready to accept data
  step(10)
  poke(dut.io.out.ready, 1)
  
  while (cntValidOut < in.size) {
    if (peek(dut.io.out.valid) == true) {
      dut.proto match {
        case dspR: DspReal => realTolDecPts.withValue(tol) { expect(dut.io.out.bits,  in(cntValidOut)) }
        case _ =>  fixTolLSBs.withValue(tol) { expect(dut.io.out.bits, in(cntValidOut)) }
      }
      if (cntValidOut == in.size - 1)
        expect(dut.io.lastOut, 1)
      cntValidOut += 1
    }
    step(1)
  }
  cntValidOut = 0
  step(2)
}

object CellUnderTestTester {
  def apply[T <: Data : Ring](proto: T, signalDepth: Int, tol: Int): Boolean = {
    val testSignal = proto match {
      case f: FixedPoint => Seq.fill(signalDepth)((Random.nextDouble() * 2 - 1) * ((1 << proto.getWidth - f.binaryPoint.get - 1)))
      case s: SInt => Seq.fill(signalDepth)((Random.nextInt((1 << (proto.getWidth - 1)) * 2) - (1 << (proto.getWidth - 1))).toDouble)
      case u: UInt => Seq.fill(signalDepth)(Random.nextInt(1 << (proto.getWidth)).toDouble)
      case d: DspReal => Seq.fill(signalDepth)((Random.nextInt(Double.MaxValue.toInt) - Double.MaxValue.toInt).toDouble)
    }
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator"),
      () => new CellUnderTest(proto)) { c =>
      new CellUnderTestTester(c, testSignal, tol)
    }
  }
}

class ShiftRegMemStreamTester[T <: Data](dut: ShiftRegisterMemStream[T], in: Seq[Double], tol: Int) extends DspTester(dut) {
  
  var cntValidOut = 0
  
  val currentDepth = dut.maxDepth
  //for (currentDepth <- 2 to dut.maxDepth) {
    poke(dut.io.in.valid, 0)
    poke(dut.io.out.ready, 0)
    poke(dut.io.depth, currentDepth)
    step(2)
    
    poke(dut.io.out.ready, 1)
    for (i <- 0 until in.size) {
      poke(dut.io.in.valid, 0)
      val delay = 3//Random.nextInt(5)
      step(delay)
      poke(dut.io.in.valid, 1)
      poke(dut.io.in.bits, in(i))
      if (i == (in.size - 1))
        poke(dut.io.lastIn, 1)
      if (peek(dut.io.out.valid) == true) {
        dut.proto match {
          case dspR: DspReal => realTolDecPts.withValue(tol) { expect(dut.io.out.bits,  in(cntValidOut)) }
          case _ =>  fixTolLSBs.withValue(tol) { expect(dut.io.out.bits, in(cntValidOut)) }
        }
        cntValidOut += 1
      }
      step(1)
    }
    poke(dut.io.lastIn, 0)
    poke(dut.io.in.valid, 0)
    poke(dut.io.out.ready, 0) // it should not shift if output is not ready to accept data
    step(10)
    poke(dut.io.out.ready, 1)
    
    while (cntValidOut < in.size) {
      if (peek(dut.io.out.valid) == true) {
        dut.proto match {
          case dspR: DspReal => realTolDecPts.withValue(tol) { expect(dut.io.out.bits,  in(cntValidOut)) }
          case _ =>  fixTolLSBs.withValue(tol) { expect(dut.io.out.bits, in(cntValidOut)) }
        }
        if (cntValidOut == in.size - 1)
          expect(dut.io.lastOut, 1)
        cntValidOut += 1
      }
      step(1)
    }
    cntValidOut = 0
    step(2)
  //}
}

object ShiftRegMemStreamTester {
  def apply[T <: Data : Ring](proto: T, maxDepth: Int, tol: Int): Boolean = {
    val testSignal = proto match {
      case f: FixedPoint => (0 until maxDepth * 2).map(c => c.toDouble) // Seq.fill(maxDepth * 2)((Random.nextDouble() * 2 - 1) * ((1 << proto.getWidth - f.binaryPoint.get - 1)))
      case s: SInt => Seq.fill(maxDepth * 2)((Random.nextInt((1 << (proto.getWidth - 1)) * 2) - (1 << (proto.getWidth - 1))).toDouble)
      case u: UInt => Seq.fill(maxDepth * 2)(Random.nextInt(1 << (proto.getWidth)).toDouble)
      case d: DspReal => Seq.fill(maxDepth * 2)((Random.nextInt(Double.MaxValue.toInt) - Double.MaxValue.toInt).toDouble)
    }
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator"),
      () => new ShiftRegisterMemStream(proto, maxDepth)) { c =>
      new ShiftRegMemStreamTester(c, testSignal, tol)
    }
  }
}

/*
class ShiftRegMemTester[T <: Data](dut: ShiftRegisterMem[T], in: Seq[Double], tol: Int) extends DspTester(dut) {
  
  var cntValidOut = 0
  
  val currentDepth = dut.maxDepth
  //for (currentDepth <- 2 to dut.maxDepth) {
    poke(dut.io.in.valid, 0)
    //poke(dut.io.out.ready, 0)
    poke(dut.io.depth.valid, 1)
    poke(dut.io.depth.bits, currentDepth)
    step(2)
    poke(dut.io.depth.valid, 0)
    step(2)
    
    poke(dut.io.in.valid, 1)
    
    for (i <- 0 until in.size) {
      poke(dut.io.in.valid, 0)
      val delay = 3 //Random.nextInt(5)
      step(delay)
      poke(dut.io.in.valid, 1)
      poke(dut.io.in.bits, in(i))
      if (peek(dut.io.out.valid) == true) {
        dut.gen match {
          case dspR: DspReal => realTolDecPts.withValue(tol) { expect(dut.io.out.bits,  in(cntValidOut)) }
          case _ =>  fixTolLSBs.withValue(tol) { expect(dut.io.out.bits, in(cntValidOut)) }
        }
        cntValidOut += 1
      }
      step(1)
    }
    
    step(2)
  //}
}

object ShiftRegMemTester {
  def apply[T <: Data : Ring](proto: T, maxDepth: Int, tol: Int): Boolean = {
    val testSignal = proto match {
      case f: FixedPoint => (0 until maxDepth * 2).map(c => c.toDouble) // Seq.fill(maxDepth * 2)((Random.nextDouble() * 2 - 1) * ((1 << proto.getWidth - f.binaryPoint.get - 1)))
      case s: SInt => Seq.fill(maxDepth * 2)((Random.nextInt((1 << (proto.getWidth - 1)) * 2) - (1 << (proto.getWidth - 1))).toDouble)
      case u: UInt => Seq.fill(maxDepth * 2)(Random.nextInt(1 << (proto.getWidth)).toDouble)
      case d: DspReal => Seq.fill(maxDepth * 2)((Random.nextInt(Double.MaxValue.toInt) - Double.MaxValue.toInt).toDouble)
    }
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator"),
      () => new ShiftRegisterMem(proto, maxDepth)) { c =>
      new ShiftRegMemTester(c, testSignal, tol)
    }
  }
}*/


class CFARUtilSpec extends FlatSpec with Matchers {

  behavior of "AdjustableShiftRegisterStream"
  
  for (maxDepth <- Seq(6)) {
    it should s"work with delay = $maxDepth and FixedPoint data input" in {
      AdjShiftRegStreamTester(FixedPoint(16.W, 5.BP), maxDepth, 2) should be (true)
    }
  }
  
  behavior of "CellUnderTest"
  
  val signalDepth = 5
  it should s"work with FixedPoint data input" in {
    CellUnderTestTester(FixedPoint(16.W, 5.BP), signalDepth, 2) should be (true)
  }
  
  behavior of "ShiftRegisterStreamMem"
  
  for (maxDepth <- Seq(4)) {
    it should s"work with delay = $maxDepth FixedPoint data input and for SyncReadMem used as memory" in {
      //CellUnderTestTester(FixedPoint(16.W, 5.BP), signalDepth, 2) should be (true)
      ShiftRegMemStreamTester(FixedPoint(16.W, 5.BP), maxDepth, 2) should be (true)
    }
  }
}
