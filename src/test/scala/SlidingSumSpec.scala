package cfar

import chisel3._
import chisel3.util._
import chisel3.experimental._
import scala.math._

import chiseltest._
//import chiseltest.internal._
//import chiseltest.simulator.{VerilatorFlags}
//import firrtl.AnnotationSeq

import scala.collection._
import scala.util.Random

import dsptools.numbers._

import org.scalatest.flatspec.AnyFlatSpec

trait SlidingSumTester {

  def compare_data(expected: Double, received: Double, tol: Double) {
    //println(abs(expected - received))
    assert(abs(expected - received) <= tol, "Mismatch!!!")
  }

  def testSlidingSum[T <: Data : Real : BinaryRepresentation](dut: SlidingSum[T], in: Seq[Double], tol: Double, runTimeDepth: Int = 3, runTimeWin: Int = 16) = {
    // this one tests only version when guardCells are equal to zero
    dut.clock.setTimeout(10000)
    require(dut.params.guardCells == 0)
    require(runTimeDepth <= dut.params.depth)
    require(runTimeDepth < runTimeWin)

    var inValid = false
    var cntInValid = 0
    var cntOutValid = 0
    var outReady = false
    val totalData = in.size
    var bin: BigInt = 0

    val depthForGoldenModel = if (dut.params.runTimeDepth) runTimeDepth else dut.params.depth
    val winForGoldenModel = if (dut.params.runTimeTestWindowSize) runTimeWin else dut.params.testWindowSize

    val expSlidingSum = CFARUtils.SlidingSum(in, depthForGoldenModel, 0)
    expSlidingSum.map { c => println(c) }

    if (dut.params.runTimeDepth) {
      dut.io.depthRunTime.get.poke(runTimeDepth)
    }
    //dut.io.guardRunTime.get.poke(dut.params.guardCells.U)

    val frac = pow(2, (dut.params.protoOut match {
      case fp: FixedPoint => fp.binaryPoint.get
      case _ => 0
    }))

    if (dut.params.runTimeTestWindowSize) {
      dut.io.windowRunTime.get.poke(dut.params.testWindowSize.U)
    }

    dut.io.in.valid.poke(false.B)
    dut.io.out.ready.poke(false.B)
    dut.clock.step()

    while (cntOutValid < totalData) {
      while (cntInValid < totalData) {
        inValid = Random.nextBoolean()  //true
        outReady = Random.nextBoolean() //true
        dut.io.in.valid.poke(inValid.B)
        dut.io.out.ready.poke(outReady.B)

        val validIn = dut.io.in.valid.peek.litToBoolean
        val readyIn = dut.io.in.ready.peek.litToBoolean

        if (validIn && readyIn) {
          val dataToPoke = Real[T].fromDouble(in(cntInValid), dut.params.protoIn)
          //println(dataToPoke)
          dut.io.in.bits.poke(dataToPoke)

          if (cntInValid == totalData-1) {
            dut.io.lastIn.poke(true.B)
          }
          else {
            dut.io.lastIn.poke(false.B)
          }
          cntInValid = cntInValid + 1
        }

        val validOut = dut.io.out.valid.peek.litToBoolean
        val readyOut = dut.io.out.ready.peek.litToBoolean

        if (validOut & readyOut) {
          val sum = dut.io.out.bits.slidingSum.peek.litValue
          println(sum.toDouble/frac)
          compare_data(expSlidingSum(cntOutValid).toDouble, sum.toDouble/frac, tol)
          if (dut.params.sendMiddle) {
            val middleCell = dut.io.out.bits.middleCell.get.peek.litValue
            compare_data(middleCell.toDouble, in(cntOutValid), tol = 0.0)
          }
          if (dut.params.sendBin) {
            bin = dut.io.out.bits.bin.get.peekInt()
            compare_data(bin.toDouble, cntOutValid.toDouble, tol = 0.0)
          }
          cntOutValid = cntOutValid + 1
        }
        dut.clock.step()
      }

      outReady =  Random.nextBoolean() //true
      dut.io.in.valid.poke(false.B)
      dut.io.lastIn.poke(false.B)
      dut.io.out.ready.poke(outReady.B)

      val validOut = dut.io.out.valid.peek.litToBoolean
      val readyOut = dut.io.out.ready.peek.litToBoolean

      if (validOut & readyOut) {
        val sum = dut.io.out.bits.slidingSum.peek.litValue
        compare_data(expSlidingSum(cntOutValid).toDouble, sum.toDouble/frac, tol)
        //println(sum.toDouble/frac)
        if (dut.params.sendMiddle) {
          val middleCell = dut.io.out.bits.middleCell.get.peek.litValue
          compare_data(middleCell.toDouble, in(cntOutValid), tol = 0.0)
        }
        if (dut.params.sendBin) {
          bin = dut.io.out.bits.bin.get.peekInt()
          compare_data(bin.toDouble, cntOutValid.toDouble, tol = 0.0)
        }
        cntOutValid = cntOutValid + 1
      }
      dut.clock.step()
    }
    dut.clock.step()
    dut.io.in.valid.poke(false.B)
    dut.io.out.ready.poke(false.B)
    dut.clock.step()
  }
}

class SlidingSumTest extends AnyFlatSpec with ChiselScalatestTester with SlidingSumTester {

  Random.setSeed(11110L)

  // UInt data type
  for (n <- Seq(5, 7, 9, 11)) {        // normally it is odd
    //for (winSize <- Seq(16)) {
    for (winSize <- Seq(16, 32, 64)) { // normally it is even
      for (runTime <- Seq(false, true)) {
        val inputProto = UInt(5.W)
        val outputProto = UInt((5 + log2Up(n)).W)
        val params: SlidingSumParams[UInt] = SlidingSumParams(
          protoIn = inputProto,
          protoOut = outputProto,
          depth = n,
          runTimeDepth = runTime,
          runTimeGuard = false,
          guardCells = 0,
          retiming = false,
          runTimeTestWindowSize = false,
          testWindowSize = winSize
        )
        val in = Seq.fill(winSize)(Random.nextInt(1<<(inputProto.getWidth)).toDouble)
        if (runTime) {
          val runTimeDepths = 3 to n by 2 // collect all odd numbers
          for (depthRt <- runTimeDepths) {
            it should f"work for UInt, n equal to $n,  winSize = $winSize, runTime = $runTime, guardCellls equal to zero, runTimeDepth = $depthRt" ignore {
              test(new SlidingSum(params)).withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut => testSlidingSum(dut, in, 0, depthRt) }
            }
          }
        }
        else {
          it should f"work for UInt, n equal to $n,  winSize = $winSize, runTime = $runTime, guardCellls equal to zero" ignore {
            test(new SlidingSum(params)).withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut => testSlidingSum(dut, in, 0) }
          }
        }
      }
    }
  }

  // Sint
  for (n <- Seq(5, 7, 9, 11)) {        // normally it is odd
    for (winSize <- Seq(16, 32, 64)) { // normally it is even
      for (runTime <- Seq(false, true)) {
        val inputProto = SInt(5.W)
        val outputProto = SInt((5 + log2Up(n)).W)
        val params: SlidingSumParams[SInt] = SlidingSumParams(
          protoIn = inputProto,
          protoOut = outputProto,
          depth = n,
          runTimeDepth = runTime,
          runTimeGuard = false,
          guardCells = 0,
          retiming = false,
          runTimeTestWindowSize = false,
          testWindowSize = winSize
        )
        val in = Seq.fill(winSize)((Random.nextInt((1<<(inputProto.getWidth-1))*2) - (1<<(inputProto.getWidth-1))).toDouble)
        if (runTime) {
          val runTimeDepths = 3 to n by 2 // collect all odd numbers
          for (depthRt <- runTimeDepths) {
            it should f"work for SInt, n equal to $n,  winSize = $winSize, runTime = $runTime, guardCellls equal to zero, runTimeDepth = $depthRt" ignore {
              test(new SlidingSum(params)).withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut => testSlidingSum(dut, in, 0, depthRt) }
            }
          }
        }
        else {
          it should f"work for SInt, n equal to $n,  winSize = $winSize, runTime = $runTime, guardCellls equal to zero" ignore {
            test(new SlidingSum(params)).withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut => testSlidingSum(dut, in, 0) }
          }
        }
      }
    }
  }


  // FixedPoint
  for (n <- Seq(5, 7, 9, 11)) {        // normally it is odd
  //for (n <- Seq(9)) {                // normally it is odd
    for (winSize <- Seq(16, 32, 64)) { // normally it is even
      for (runTime <- Seq(true)) {
        val inputProto = FixedPoint(10.W, 8.BP)
        val outputProto = FixedPoint((10 + log2Up(n)+1).W, 8.BP)
        val params: SlidingSumParams[FixedPoint] = SlidingSumParams(
          protoIn = inputProto,
          protoOut = outputProto,
          depth = n,
          runTimeDepth = runTime,
          runTimeGuard = false,
          guardCells = 0,
          retiming = false,
          runTimeTestWindowSize = false,
          testWindowSize = winSize
        )
        val in: Seq[Double] = Seq.fill(winSize)((Random.nextDouble()*2-1) * ((1<<inputProto.getWidth - inputProto.binaryPoint.get-1)))
        if (runTime) {
          val runTimeDepths = 3 to n by 2 // collect all odd numbers
          for (depthRt <- runTimeDepths) {
            it should f"work for FixedPoint, n equal to $n,  winSize = $winSize, runTime = $runTime, guardCellls equal to zero, runTimeDepth = $depthRt" in {
              test(new SlidingSum(params)).withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut => testSlidingSum(dut, in, 0.5, depthRt) }
            }
          }
        }
        else {
          it should f"work for FixedPoint, n equal to $n,  winSize = $winSize, runTime = $runTime, guardCellls equal to zero" ignore {
            test(new SlidingSum(params)).withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut => testSlidingSum(dut, in, 0.5) }
          }
        }
      }
    }
  }

  //FixedPoint
  /*it should f"work for UInt, depth equal to 7, guardCells equal to zero, windowTestSize equal to 256" in {
    val inputProto = UInt(5.W)
    val outputProto = UInt((5 + log2Up(7)).W)
    val in = Seq.fill(n)(Random.nextInt(1<<(inputProto.getWidth)).toDouble)
    //in.map { c => println(c) }

    val params: SlidingSumParams[UInt] = SlidingSumParams(
      protoIn = inputProto,
      protoOut = outputProto,
      depth = 9,
      runTimeDepth = true,
      runTimeGuard = false,
      guardCells = 0,
      retiming = false,
      runTimeTestWindowSize = true,
      testWindowSize = n
    )
    test(new SlidingSum(params)).withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut => testSlidingSum(dut, in, 0) }
  }*/
}
