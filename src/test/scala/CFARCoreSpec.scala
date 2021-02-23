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

class CFARThrPlotTester[T <: Data](dut: CFARCore[T],
                                   in: Seq[Double],
                                   thrFactor: Double,
                                   cfarMode: Int,
                                   expPeaks: Seq[Int],
                                   //thrPlot: Boolean,
                                   expThr: Seq[Double], 
                                   tol: Int) extends DspTester(dut) {
  require(dut.params.leadLaggWindowSize > 3 && dut.params.guardWindowSize > 1)
  var cntValidOut = 0
  var threshold: ArrayBuffer[Double] = mutable.ArrayBuffer[Double]()

  poke(dut.io.in.valid, 0)
  poke(dut.io.out.ready, 0)
  // initilize control registers
  poke(dut.io.fftWin, dut.params.fftSize)
  poke(dut.io.thresholdScaler, thrFactor)
  poke(dut.io.peakGrouping, 0)
  
  if (dut.params.CFARAlgorithm == GOSCACFARType) {
    poke(dut.io.cfarAlgorithm.get, 0)
  }

  if (dut.params.includeCASH) {
    poke(dut.io.subCells.get, dut.params.minSubWindowSize.get) // this should gave the same result as CA CFAR
  }

  poke(dut.io.cfarMode, cfarMode)
  poke(dut.io.logOrLinearMode, 1)
   
  //for (lWinSize <- 4 to dut.params.leadLaggWindowSize) {
  // for (guardSize <- 2 to dut.params.guardWindowSize) {
      // comment this part here if you want to use values defined inside 'for' expression
      val lWinSize = dut.params.leadLaggWindowSize
      val guardSize = dut.params.guardWindowSize
      println(s"Testing CFAR core with lWinSize = $lWinSize and guardSize = $guardSize")
      
      poke(dut.io.windowCells, lWinSize)
      
      if (cfarMode == 3) {
        poke(dut.io.guardCells, 0)
      }
      else {
        poke(dut.io.guardCells, guardSize)
      }

      if (dut.params.CFARAlgorithm != GOSCFARType) {
        poke(dut.io.divSum.get, log2Ceil(lWinSize))
      }
      if (dut.params.CFARAlgorithm != CACFARType) {
        poke(dut.io.indexLead.get, lWinSize/2)
        poke(dut.io.indexLagg.get, lWinSize/2)
      }
      step(2) // be sure that control registers are first initilized and then set ready and valid signals
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
            case dspR: DspReal => {
              realTolDecPts.withValue(tol) { expect(dut.io.out.bits.cut.get,  in(cntValidOut).toDouble) }
              realTolDecPts.withValue(tol) { expect(dut.io.out.bits.threshold,  expThr(cntValidOut)) }
            }
            case _ =>  {
              fixTolLSBs.withValue(tol) { expect(dut.io.out.bits.cut.get, in(cntValidOut)) }
              fixTolLSBs.withValue(tol) { expect(dut.io.out.bits.threshold, expThr(cntValidOut)) }
            }
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

      while (cntValidOut < in.size) {
        if (peek(dut.io.out.valid) && peek(dut.io.out.ready)) {
          //expect(dut.io.out.bits.cut,  in(cntValidOut))
          dut.params.protoIn match {
            case dspR: DspReal => {
              realTolDecPts.withValue(tol) { expect(dut.io.out.bits.cut.get, in(cntValidOut)) }
              realTolDecPts.withValue(tol) { expect(dut.io.out.bits.threshold, expThr(cntValidOut)) }
            }
            case _ =>  {
              fixTolLSBs.withValue(tol) { expect(dut.io.out.bits.cut.get, in(cntValidOut)) }
              fixTolLSBs.withValue(tol) { expect(dut.io.out.bits.threshold, expThr(cntValidOut)) }
            }
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

  //  if (thrPlot) {
      val f = Figure()
      val p = f.subplot(0)
      p.legend_=(true)
      val xaxis = (0 until dut.params.fftSize).map(e => e.toDouble).toSeq.toArray
      p.xlabel = "Frequency bin"
      p.ylabel = "Amplitude"

      val fftPlot = in.toSeq
      val thresholdPlot = threshold.toSeq

      p += plot(xaxis, fftPlot.toArray, name = "Signal")
      p += plot(xaxis, thresholdPlot.toArray, name = "CFAR threshold")
      p.title_=(s"Constant False Alarm Rate")

      f.saveas(s"test_run_dir/CFARThresholdPlot.pdf")
  //  }
}

// Simple tester, checks only data stream functionality
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
              case dspR: DspReal => realTolDecPts.withValue(tol) { expect(dut.io.out.bits.cut.get,  in(cntValidOut).toDouble) }
              case _ =>  fixTolLSBs.withValue(tol) { expect(dut.io.out.bits.cut.get, in(cntValidOut)) }
            }
            println(peek(dut.io.out.bits.cut.get).toString)
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
            case dspR: DspReal => realTolDecPts.withValue(tol) { expect(dut.io.out.bits.cut.get,  in(cntValidOut)) }
            case _ =>  fixTolLSBs.withValue(tol) { expect(dut.io.out.bits.cut.get, in(cntValidOut)) }
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
  def apply[T <: Data : Real: BinaryRepresentation](params: CFARParams[T],
                                                    cfarMode: String = "Smallest Of",
                                                    thrFactor: Double = 3.5,
                                                    tol: Int = 3,
                                                    random: Boolean = false,
                                                    thrPlot: Boolean = false,
                                                    backend: String = "verilator"): Boolean = {
    require(params.fftSize > 8)
    if (random == false) {
      val numSamples = params.fftSize
      Random.setSeed(11110L) // make same random data vector for every test case
      val noise = (0 until numSamples).map(i => Complex(math.sqrt(Random.nextDouble + Random.nextDouble),0))
      val s1    = (0 until numSamples).map(i => Complex(0.4 * math.cos(2 * math.Pi * 1/8 * i), 0.4 * math.sin(2 * math.Pi * 1/8 * i)))
      val s2    = (0 until numSamples).map(i => Complex(0.2 * math.cos(2 * math.Pi * 1/4 * i), 0.2 * math.sin(2 * math.Pi * 1/4 * i)))
      val s3    = (0 until numSamples).map(i => Complex(0.1 * math.cos(2 * math.Pi * 1/2 * i), 0.1 * math.sin(2 * math.Pi * 1/2 * i)))

      // can be simplified
      var sum   = noise.zip(s1).map { case (a, b) => a + b}.zip(s2).map{ case (c, d) => c + d }.zip(s3).map{ case (e, f)  => e + f }

      val fft = fourierTr(DenseVector(sum.toArray)).toScalaVector
      val testSignal = fft.map(c => math.sqrt(pow(c.real,2) + pow(c.imag,2))) // try c.abs

      //val testSignal = (0 until params.fftSize).map(c => c.toDouble)

      val cfarModeNum = cfarMode match {
                          case "Cell Averaging" => 0
                          case "Greatest Of" => 1
                          case "Smallest Of" => 2
                          case "CASH" => 3
                          case _ => 0
                        }
      val considerEdges = if (params.includeCASH == true) false else true

      val (expThr, expPeaks) = if (params.includeCASH && cfarMode == "CASH")
                                  CFARUtils.cfarCASH(testSignal, referenceCells = params.leadLaggWindowSize, subCells = params.minSubWindowSize.get, scalingFactor = thrFactor, plotEn = true)
                               else
                                  CFARUtils.cfarCA(testSignal, cfarMode = cfarMode, referenceCells = params.leadLaggWindowSize, guardCells = params.guardWindowSize, considerEdges = considerEdges, scalingFactor = thrFactor, plotEn = true)

      chisel3.iotesters.Driver.execute(Array("-tbn", backend),
        () => new CFARCore(params)) { c =>
        new CFARThrPlotTester(c, testSignal, cfarMode = cfarModeNum, thrFactor = thrFactor, expThr = expThr, expPeaks = expPeaks, tol = tol)
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
    leadLaggWindowSize = 16,
    guardWindowSize = 5,
    fftSize = 64
    //pipes are zero
  )
  
  // works good!
  it should s"test streaming data core with no pipes" ignore {
    CFARTester(paramsNoPipes, tol = 2) should be (true)
  }
  
  val paramsWithPipes: CFARParams[FixedPoint] = CFARParams(
    protoIn = FixedPoint(16.W, 8.BP),
    protoThreshold = FixedPoint(16.W, 8.BP),
    protoScaler = FixedPoint(16.W, 8.BP),
    leadLaggWindowSize = 16,
    guardWindowSize = 5,
    fftSize = 64,
    numAddPipes = 2,
    numMulPipes = 2
  )
  // works good!
  it should s"test streaming data with pipes" ignore {
    CFARTester(paramsWithPipes, tol = 2) should be (true)
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
    CFARTester(paramsTestSum, tol = 2) should be (true)
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
  
  it should s"plot CFAR threshold for CFAR core with LIS" ignore { // ignore it because of travis
    CFARTester(paramsFixedLIS, tol = 2, thrPlot = false) should be (true)
  }
  
  val paramsFixedMem: CFARParams[FixedPoint] = CFARParams(
    protoIn = FixedPoint(16.W, 8.BP),//DspReal(),
    protoThreshold = FixedPoint(16.W, 8.BP),//DspReal(),
    protoScaler = FixedPoint(16.W, 8.BP), //DspReal(),
    leadLaggWindowSize = 4,//16,
    guardWindowSize = 2,
    fftSize = 64,//512,
    CFARAlgorithm = CACFARType,
    numAddPipes = 0,
    numMulPipes = 0
  )
  
  for (cfarMode <- Seq("Cell Averaging")) {//, "Greatest Of", "Cell Averaging")) {
    it should s"plot CFAR threshold for CFAR core with Mem and FixedPoint data type and cfarMode = $cfarMode" ignore { // ignore it because of travis
      CFARTester(paramsFixedMem, tol = 3, cfarMode = cfarMode, thrPlot = true) should be (true)
    }
  }

  val paramsDspRealMem: CFARParams[DspReal] = CFARParams(
    protoIn = DspReal(),
    protoThreshold = DspReal(),
    protoScaler = DspReal(),
    leadLaggWindowSize = 8,//16,
    guardWindowSize = 2,
    fftSize = 64,//512,
    CFARAlgorithm = CACFARType,
    numAddPipes = 0,
    numMulPipes = 0
  )

  for (cfarMode <- Seq("Cell Averaging")) {//, "Greatest Of", "Cell Averaging")) {
    it should s"plot CFAR threshold for CFAR core with Mem and DspReal data type and cfarMode = $cfarMode" ignore {
      CFARTester(paramsDspRealMem, tol = 12, cfarMode = cfarMode, thrPlot = true) should be (true)
    }
  }

  val paramsFixedCASH: CFARParams[FixedPoint] = CFARParams(
    protoIn = FixedPoint(16.W, 6.BP),//DspReal(),
    protoThreshold = FixedPoint(16.W, 6.BP),//DspReal(),
    protoScaler = FixedPoint(16.W, 6.BP),
    leadLaggWindowSize = 16,
    minSubWindowSize = Some(4),
    guardWindowSize = 2,
    includeCASH = true,
    fftSize = 512,
    CFARAlgorithm = CACFARType,
    numAddPipes = 0,
    numMulPipes = 0
  )

  it should s"plot CFAR threshold for CFAR  CASH and FixedPoint data type" ignore {
    CFARTester(paramsFixedCASH, tol = 3, cfarMode = "CASH", thrPlot = true) should be (true)
  }
  
  
  val paramsFixedCASHTest: CFARParams[FixedPoint] = CFARParams(
    protoIn = FixedPoint(16.W, 6.BP), //DspReal(),
    protoThreshold = FixedPoint(16.W, 6.BP), //DspReal(),
    protoScaler = FixedPoint(16.W, 6.BP),
    leadLaggWindowSize = 16,
    minSubWindowSize = Some(4),
    guardWindowSize = 2,
    includeCASH = true,
    fftSize = 64, // 512
    CFARAlgorithm = CACFARType,
    numAddPipes = 0,
    numMulPipes = 0
  )
  it should s"plot CFAR threshold for CFAR CASH test" in {
    CFARTester(paramsFixedCASHTest, tol = 3, cfarMode = "CASH", thrPlot = true) should be (true)
  }

  val paramsDspRealCASH: CFARParams[DspReal] = CFARParams(
    protoIn = DspReal(),
    protoThreshold = DspReal(),
    protoScaler = DspReal(),
    leadLaggWindowSize = 16,
    minSubWindowSize = Some(4),
    guardWindowSize = 2,
    includeCASH = true,
    fftSize = 512,
    CFARAlgorithm = CACFARType,
    numAddPipes = 0,
    numMulPipes = 0
  )
  it should s"plot CFAR threshold for CFAR  CASH and DspReal data type" ignore {
    CFARTester(paramsDspRealCASH, tol = 12, cfarMode = "Smallest Of", thrPlot = true) should be (true)
  }
}
