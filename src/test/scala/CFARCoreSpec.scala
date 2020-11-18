package cfar

import chisel3._
import chisel3.util._
import chisel3.experimental._

import dsptools._
import dsptools.numbers._
import scala.math.{Pi, pow}
import scala.collection._
import scala.collection.mutable.ArrayBuffer

import breeze.math.Complex
import breeze.signal.{fourierTr, iFourierTr}
import breeze.linalg._
import breeze.plot._

import org.scalatest.{FlatSpec, Matchers}
import scala.util.Random

class CFARThrPlotTester[T <: Data](dut: CFARCore[T], in: Seq[Double], expPeaks: Seq[Int], tol: Int) extends DspTester(dut) {
  require(dut.params.leadLaggWindowSize > 3 && dut.params.guardWindowSize > 1)
  var cntValidOut = 0
  var threshold: ArrayBuffer[Double] = mutable.ArrayBuffer[Double]()
  
  
  poke(dut.io.in.valid, 0)
  poke(dut.io.out.ready, 0)
  // initilize control registers
  poke(dut.io.fftWin, dut.params.fftSize)
  poke(dut.io.thresholdScaler, 3.5)
  poke(dut.io.peakGrouping, 0)
  
  if (dut.params.CFARAlgorithm == GOSCACFARType) {
    poke(dut.io.cfarAlgorithm.get, 0)
  }
  poke(dut.io.cfarMode, 3)
  poke(dut.io.logOrLinearMode, 1)
   
  //for (lWinSize <- 4 to dut.params.leadLaggWindowSize) {
  //  for (guardSize <- 2 to dut.params.guardWindowSize) {
   val lWinSize = dut.params.leadLaggWindowSize
   val guardSize = dut.params.guardWindowSize
      println(s"Testing CFAR core with lWinSize = $lWinSize and guardSize = $guardSize")
      
      poke(dut.io.windowCells, lWinSize)
      poke(dut.io.guardCells, guardSize)
      if (dut.params.CFARAlgorithm != GOSCFARType) {
        poke(dut.io.divSum.get, log2Ceil(lWinSize))
      }
      if (dut.params.CFARAlgorithm != CACFARType) {
        poke(dut.io.indexLead.get, lWinSize/2)
        poke(dut.io.indexLagg.get, lWinSize/2)
      }
      step(2) // be sure that control registers are first initilized and then run ready and valid signals
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
          dut.params.protoIn match {
            case dspR: DspReal => realTolDecPts.withValue(tol) { expect(dut.io.out.bits.cut,  in(cntValidOut).toDouble) }
            case _ =>  fixTolLSBs.withValue(tol) { expect(dut.io.out.bits.cut, in(cntValidOut)) }
          }
          //fftBin
          if (expPeaks.contains(peek(dut.io.fftBin))) {
            expect(dut.io.out.bits.peak, 1)
          }
          cntValidOut += 1
          threshold += peek(dut.io.out.bits.threshold)
        }
        step(1)
      }
      poke(dut.io.lastIn, 0)
      poke(dut.io.in.valid, 0)
      poke(dut.io.out.ready, 0)
      step(10)
      poke(dut.io.out.ready, 1)
      
      step(50)
      println(cntValidOut.toString)
      while (cntValidOut < in.size-1) {
        if (peek(dut.io.out.valid) == true && peek(dut.io.out.ready)) {
          //expect(dut.io.out.bits.cut,  in(cntValidOut))
          dut.params.protoIn match {
            case dspR: DspReal => realTolDecPts.withValue(tol) { expect(dut.io.out.bits.cut,  in(cntValidOut)) }
            case _ =>  fixTolLSBs.withValue(tol) { expect(dut.io.out.bits.cut, in(cntValidOut)) }
          }
          if (expPeaks.contains(peek(dut.io.fftBin))) {
            expect(dut.io.out.bits.peak, 1)
          }
          threshold += peek(dut.io.out.bits.threshold)
          if (cntValidOut == in.size - 1)
            expect(dut.io.lastOut, 1)
          cntValidOut += 1
        }
        step(1)
      }
      cntValidOut = 0
      step(dut.params.leadLaggWindowSize * 2)
      // here plot threshold and input
     
      val f = Figure()
      val p = f.subplot(0)
      p.legend_=(true)
      val xaxis = (0 until dut.params.fftSize).map(e => e.toDouble).toSeq.toArray
      p.xlabel = "Frequency bin"
      p.ylabel = "Amplitude"
      
      val fftPlot = in.toSeq
      val thresholdPlot = threshold.toSeq
      
      p += plot(xaxis, fftPlot.toArray, name = "Signal")
      p += plot(xaxis, thresholdPlot.toArray, name = "CFAR threshold") //'.'
      p.title_=(s"Constant False Alarm Rate")
      
      f.saveas(s"test_run_dir/CFARThresholdPlot.pdf")
    }
   //}
//}


//Simple tester, checks only data stream functionality
class CFARStreamTester[T <: Data](dut: CFARCore[T], in: Seq[Double], tol: Int) extends DspTester(dut) {
  require(dut.params.leadLaggWindowSize > 3 && dut.params.guardWindowSize > 1)
  var cntValidOut = 0
  //Random.setSeed(11110L)

  
  poke(dut.io.in.valid, 0)
  poke(dut.io.out.ready, 0)
  // initilize control registers
  poke(dut.io.fftWin, 64)
  poke(dut.io.thresholdScaler, 0.5)
  if (dut.params.CFARAlgorithm == GOSCACFARType) {
    poke(dut.io.cfarAlgorithm.get, 0)
  }
  poke(dut.io.peakGrouping, 0)
  poke(dut.io.cfarMode, 0)
   
 // for (lWinSize <- 4 to dut.params.leadLaggWindowSize) {
   // for (guardSize <- 2 to dut.params.guardWindowSize) {
      val lWinSize = dut.params.leadLaggWindowSize
      val guardSize = dut.params.guardWindowSize
      println(s"Testing CFAR core with lWinSize = $lWinSize and guardSize = $guardSize")
      
      poke(dut.io.windowCells, lWinSize)
      poke(dut.io.guardCells, guardSize)
      if (dut.params.CFARAlgorithm != CACFARType) {
        poke(dut.io.indexLead.get, lWinSize/2)
        poke(dut.io.indexLagg.get, lWinSize/2)
      }
      if (dut.params.CFARAlgorithm != GOSCFARType) {
        poke(dut.io.divSum.get, log2Ceil(lWinSize))
      }
      
      step(2) // be sure that control registers are first initilized and then run ready and valid signals
      poke(dut.io.out.ready, 1)
      
      for (i <- 0 until in.size) {
        poke(dut.io.in.valid, 0)
        val delay = Random.nextInt(5)
        
        // just try to figure out how this functions
        for (i<-0 to delay) {
          if (peek(dut.io.out.valid) == true) {
            dut.params.protoIn match {
              case dspR: DspReal => realTolDecPts.withValue(tol) { expect(dut.io.out.bits.cut,  in(cntValidOut).toDouble) }
              case _ =>  fixTolLSBs.withValue(tol) { expect(dut.io.out.bits.cut, in(cntValidOut)) }
            }
            println(peek(dut.io.out.bits.cut).toString)
            cntValidOut += 1
          }
          step(1)
        }
        poke(dut.io.in.valid, 1)
        poke(dut.io.in.bits, in(i))
        if (i == (in.size - 1))
          poke(dut.io.lastIn, 1)
        step(1)
      }
      poke(dut.io.lastIn, 0)
      poke(dut.io.in.valid, 0)
      poke(dut.io.out.ready, 0)
      step(10)
      poke(dut.io.out.ready, 1)
      println(cntValidOut.toString)
      
      while (cntValidOut < in.size) {
        if (peek(dut.io.out.valid)) {
          //expect(dut.io.out.bits.cut,  in(cntValidOut))
          dut.params.protoIn match {
            case dspR: DspReal => realTolDecPts.withValue(tol) { expect(dut.io.out.bits.cut,  in(cntValidOut)) }
            case _ =>  fixTolLSBs.withValue(tol) { expect(dut.io.out.bits.cut, in(cntValidOut)) }
          }
          if (cntValidOut == in.size - 1)
            expect(dut.io.lastOut, 1)
          cntValidOut += 1
        }
        step(1)
      }
      cntValidOut = 0
      step(dut.params.leadLaggWindowSize * 2)
   //}
  //}
}


object CFARTester {
  def apply[T <: Data : Real: BinaryRepresentation](params: CFARParams[T], tol: Int, thrPlot: Boolean = false): Boolean = {
    require(params.fftSize > 8)
    if (thrPlot == true) {
      val numSamples = params.fftSize
      //Random.setSeed(11110L)
      val noise = (0 until numSamples).map(i => Complex(math.sqrt(Random.nextDouble + Random.nextDouble),0))
      val s1    = (0 until numSamples).map(i => Complex(0.4 * math.cos(2 * math.Pi * 1/2 * i), 0.4 * math.sin(2 * math.Pi * 1/2 * i)))
      val s2    = (0 until numSamples).map(i => Complex(0.1 * math.cos(2 * math.Pi * 1/4 * i), 0.1 * math.sin(2 * math.Pi * 1/4 * i)))
      val s3    = (0 until numSamples).map(i => Complex(0.2 * math.cos(2 * math.Pi * 1/8 * i), 0.2 * math.sin(2 * math.Pi * 1/8 * i)))
      
      // can be simplified
      val sum   = noise.zip(s1).map { case (a, b) => a + b}.zip(s2).map{ case (c, d) => c + d }.zip(s3).map{ case (e, f)  => e + f }
      
      val fft = fourierTr(DenseVector(sum.toArray)).toScalaVector
      val testSignal = fft.map(c => math.sqrt(pow(c.real,2) + pow(c.imag,2))) // try c.abs
      
      chisel3.iotesters.Driver.execute(Array("-tbn", "verilator"),
        () => new CFARCore(params)) { c =>
        new CFARThrPlotTester(c, testSignal, Seq(numSamples/2, numSamples/4, numSamples/8), tol)
      }
    }
    else {
      val testSignal = params.protoIn match {
        //case f: FixedPoint => Seq.fill(params.fftSize)((Random.nextDouble() * 2 - 1) * ((1 << params.protoIn.getWidth - f.binaryPoint.get - 1)))
        // Only positive
//         case f: FixedPoint => Seq.fill(params.fftSize)((Random.nextDouble()) * ((1 << params.protoIn.getWidth - f.binaryPoint.get - 1)))
        case f: FixedPoint => (0 until params.fftSize).map(c => c.toDouble)
        case s: SInt => Seq.fill(params.fftSize)((Random.nextInt((1 << (params.protoIn.getWidth - 1)) * 2) - (1 << (params.protoIn.getWidth - 1))).toDouble)
        case u: UInt => Seq.fill(params.fftSize)(Random.nextInt(1 << (params.protoIn.getWidth)).toDouble)
        case d: DspReal => Seq.fill(params.fftSize)((Random.nextInt(Double.MaxValue.toInt) - Double.MaxValue.toInt).toDouble)
      }
      chisel3.iotesters.Driver.execute(Array("-tbn", "verilator"),
        () => new CFARCore(params)) { c =>
        new CFARStreamTester(c, testSignal, tol)
      }
    }
  }
}


class CFARSpec extends FlatSpec with Matchers {
  behavior of "CFARCoreWithLis"
  
  val paramsNoPipes: CFARParams[FixedPoint] = CFARParams(
    protoIn = FixedPoint(16.W, 8.BP),
    protoThreshold = FixedPoint(16.W, 8.BP),
    protoScaler = FixedPoint(16.W, 8.BP),
    leadLaggWindowSize = 10,
    guardWindowSize = 5,
    fftSize = 64
    //pipes are zero
  )
  
  // works good!
  it should s"test streaming data core with no pipes" ignore {
    CFARTester(paramsNoPipes, 2) should be (true)
  }
  
  val paramsWithPipes: CFARParams[FixedPoint] = CFARParams(
    protoIn = FixedPoint(16.W, 8.BP),
    protoThreshold = FixedPoint(16.W, 8.BP),
    protoScaler = FixedPoint(16.W, 8.BP),
    leadLaggWindowSize = 10,
    guardWindowSize = 5,
    fftSize = 64,
    numAddPipes = 2,
    numMulPipes = 2
  )
  // works good!
  it should s"test streaming data with pipes" ignore {
    CFARTester(paramsWithPipes, 2) should be (true)
  }
  
  // it should test sliding sum 
  
  val paramsTestSum: CFARParams[FixedPoint] = CFARParams(
    protoIn = FixedPoint(16.W, 8.BP),
    protoThreshold = FixedPoint(16.W, 8.BP),
    protoScaler = FixedPoint(16.W, 8.BP),
    leadLaggWindowSize = 4,
    guardWindowSize = 2,
    fftSize = 64,
    numAddPipes = 0,
    numMulPipes = 0
  )
  // works good!
  it should s"test sliding sum" ignore {
    CFARTester(paramsTestSum, 2, false) should be (true)
  }
  
//   val paramsTestSumWithPipes: CFARParams[FixedPoint] = CFARParams(
//     protoIn = FixedPoint(16.W, 8.BP),
//     protoThreshold = FixedPoint(16.W, 8.BP),
//     protoScaler = FixedPoint(16.W, 8.BP),
//     leadLaggWindowSize = 4,
//     guardWindowSize = 2,
//     fftSize = 64,
//     numAddPipes = 2,
//     numMulPipes = 2
//   )
//   // works good!
//   it should s"test sliding sum with pipes" in {
//     CFARTester(paramsTestSumWithPipes, 2) should be (true)
//   }
  val paramsFixedLIS: CFARParams[FixedPoint] = CFARParams(
    protoIn = FixedPoint(16.W, 4.BP),//DspReal(),
    protoThreshold = FixedPoint(16.W, 4.BP),//DspReal(),
    protoScaler = FixedPoint(16.W, 4.BP), //DspReal(),
    leadLaggWindowSize = 8,
    guardWindowSize = 2,
    fftSize = 64,
//    CFARAlgorithm = CACFARType,
    numAddPipes = 0,
    numMulPipes = 1
  )
  
  it should s"plot CFAR threshold for CFAR core with LIS" in { // ignore it because of travis
    CFARTester(paramsFixedLIS, 2, false) should be (true)
  }
  
  val paramsFixedMem: CFARParams[FixedPoint] = CFARParams(
    protoIn = FixedPoint(16.W, 4.BP),//DspReal(),
    protoThreshold = FixedPoint(16.W, 4.BP),//DspReal(),
    protoScaler = FixedPoint(16.W, 4.BP), //DspReal(),
    leadLaggWindowSize = 4,
    guardWindowSize = 2,
    fftSize = 32,
    CFARAlgorithm = CACFARType,
    numAddPipes = 0,
    numMulPipes = 1
  )
  
  it should s"plot CFAR threshold for CFAR core with Mem" ignore { // ignore it because of travis
    CFARTester(paramsFixedMem, 2, true) should be (true)
  }
  
}
