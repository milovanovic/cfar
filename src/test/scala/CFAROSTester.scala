package cfar

import chisel3._
import chisel3.util._
import chisel3.experimental._

import chiseltest._
import chiseltest.internal._
import chiseltest.simulator.{VerilatorFlags}

import firrtl.AnnotationSeq

import scala.collection._
import scala.math.{Pi, pow}
import scala.util.Random
import scala.collection.mutable.ArrayBuffer

import breeze.linalg._
import breeze.math.Complex
import breeze.signal.{fourierTr}

import dsptools._
import dsptools.numbers._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

trait AbstractCFARTest {
  def testCFAR[T <: Data : Real : BinaryRepresentation](dut: CFARCoreOS[T], in: Seq[Double], indexOS: Int, windowCells: Int, thresholdScaler: Int = 1, tol: Double = 0.5) = {

    dut.clock.setTimeout(8000)

    var inValid = false
    var cntInValid = 0
    var cntOutValid = 0
    var peekedVal: BigInt = 0
    var outReady = false
    var totalData = in.size

    val (expThr, expPeaks) = CFARUtils.cfarOSlis(in, windowCells, thresholdScaler, indexOS)
    //expThr.map { c => println(c) }

    dut.io.windowCells.poke(windowCells.U)
    dut.io.fftWin.poke(dut.params.fftSize.U)
    dut.io.indexOS.get.poke(indexOS.U)
    dut.io.thresholdScaler.poke(Real[T].fromDouble(thresholdScaler, dut.params.protoScaler))

    dut.io.in.valid.poke(false.B)
    dut.io.out.ready.poke(false.B)
    dut.clock.step()

    while (cntOutValid < totalData) {
      while (cntInValid < totalData) {
        inValid = Random.nextBoolean()
        outReady = Random.nextBoolean()
        dut.io.in.valid.poke(inValid.B)
        dut.io.out.ready.poke(outReady.B)

        var validIn = dut.io.in.valid.peek.litToBoolean
        var readyIn = dut.io.in.ready.peek.litToBoolean

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

        var validOut = dut.io.out.valid.peek.litToBoolean
        var readyOut = dut.io.out.ready.peek.litToBoolean

        if (validOut & readyOut) {
          var cutToPeek = Real[T].fromDouble(0.0, dut.params.protoIn)
          var thrToPeek = Real[T].fromDouble(0.0, dut.params.protoThreshold)

          cutToPeek = dut.io.out.bits.cut.get.peek()
          thrToPeek = dut.io.out.bits.threshold.peek()

          dut.io.out.bits.cut.get.expect(Real[T].fromDouble(in(cntOutValid), dut.params.protoIn))
          dut.io.out.bits.threshold.expect(Real[T].fromDouble(expThr(cntOutValid), dut.params.protoThreshold))

          cntOutValid = cntOutValid + 1
        }
        dut.clock.step()
      }

      outReady = true //Random.nextBoolean()
      dut.io.in.valid.poke(false.B)
      dut.io.lastIn.poke(false.B)
      dut.io.out.ready.poke(outReady.B)

      var validOut = dut.io.out.valid.peek.litToBoolean
      var readyOut = dut.io.out.ready.peek.litToBoolean

      if (validOut & readyOut) {
        var cutToPeek = Real[T].fromDouble(0.0, dut.params.protoIn)
        var thrToPeek = Real[T].fromDouble(0.0, dut.params.protoThreshold)

        cutToPeek = dut.io.out.bits.cut.get.peek()
        thrToPeek = dut.io.out.bits.threshold.peek()

        dut.io.out.bits.cut.get.expect(Real[T].fromDouble(in(cntOutValid), dut.params.protoIn))
        dut.io.out.bits.threshold.expect(Real[T].fromDouble(expThr(cntOutValid), dut.params.protoThreshold))

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

class CFAROSTest extends AnyFlatSpec with ChiselScalatestTester with AbstractCFARTest {


  var testSignal: Seq[Double] = Seq()

  val params: CFARParams[FixedPoint] = CFARParams(
              protoIn = FixedPoint(16.W, 6.BP),
              protoThreshold = FixedPoint(16.W, 6.BP),
              protoScaler = FixedPoint(16.W, 6.BP),
              leadLaggWindowSize = 8,
              guardWindowSize = 0, // it should be an option potentially
              fftSize = 128,
              minSubWindowSize = None,
              includeCASH = false,
             // numMulPipes = 1,
              CFARAlgorithm = OSCFARType
            )
  val random = false
  if (random == false) {
    val numSamples = params.fftSize
    Random.setSeed(11110L)

    val noise = (0 until numSamples).map(i => Complex(math.sqrt(Random.nextDouble + Random.nextDouble),0))
    val s1    = (0 until numSamples).map(i => Complex(0.4 * math.cos(2 * math.Pi * 1/8 * i), 0.4 * math.sin(2 * math.Pi * 1/8 * i)))
    val s2    = (0 until numSamples).map(i => Complex(0.2 * math.cos(2 * math.Pi * 1/4 * i), 0.2 * math.sin(2 * math.Pi * 1/4 * i)))
    val s3    = (0 until numSamples).map(i => Complex(0.1 * math.cos(2 * math.Pi * 1/2 * i), 0.1 * math.sin(2 * math.Pi * 1/2 * i)))

    var sum   = noise.zip(s1).map { case (a, b) => a + b}.zip(s2).map{ case (c, d) => c + d }.zip(s3).map{ case (e, f)  => e + f }
    val fft = fourierTr(DenseVector(sum.toArray)).toScalaVector
    testSignal = fft.map(c => math.sqrt(pow(c.real,2) + pow(c.imag,2)))
  }
  else {
    val testSignaltmp = params.protoIn match {
      case f: FixedPoint => (0 until params.fftSize).map(c => c.toDouble)
      case d: DspReal => Seq.fill(params.fftSize)((Random.nextInt(Double.MaxValue.toInt) - Double.MaxValue.toInt).toDouble)
    }
    testSignal = testSignaltmp
  }

  //testSignal.map { c => println(c) }

  //PrintPeekPoke
  it should "test CFAR" in {
    test(new CFARCoreOS(params)).withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut => testCFAR(dut, testSignal, 2, 8) }
  }
}
