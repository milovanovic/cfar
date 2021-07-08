package cfar

import chisel3._
import chisel3.util._
import chisel3.experimental._

import dsptools._
import dsptools.numbers._

import scala.collection._
import scala.math.{Pi, pow}
import scala.util.Random
import scala.collection.mutable.ArrayBuffer

import breeze.plot._
import breeze.linalg._
import breeze.math.Complex
import breeze.signal.{fourierTr, iFourierTr}

import org.scalatest.{FlatSpec, Matchers}

class CFARGOSCATester[T <: Data](dut: CFARCore[T],
                                      in: Seq[Double],
                                      thrFactor: Double,
                                      cfarMode: String,
                                      cfarAlgorithm: String,
                                      thrPlot: Boolean,
                                      indexLagg: Int,
                                      indexLead: Int,
                                      considerEdges: Boolean,
                                      runTime: Boolean,
                                      tol: Int) extends DspTester(dut) {

  require(dut.params.leadLaggWindowSize > 3 && dut.params.guardWindowSize > 1, "Check configuration of leading/lagging cells and guard cells")
  require(dut.params.CFARAlgorithm == GOSCACFARType || dut.params.CFARAlgorithm == GOSCFARType, "Not appropriate CFAR type")
  require(dut.params.sendCut == true, "Output port for cell under test should be enabled")
  
  var cntValidOut = 0
  var threshold: ArrayBuffer[Double] = mutable.ArrayBuffer[Double]()
  var indexLeadTmp: Int = dut.params.leadLaggWindowSize/2
  var indexLaggTmp: Int = dut.params.leadLaggWindowSize/2
  
  updatableDspVerbose.withValue(false) {
    poke(dut.io.in.valid, 0)
    poke(dut.io.out.ready, 0)
    // initilize control registers
    poke(dut.io.fftWin, dut.params.fftSize)
    poke(dut.io.thresholdScaler, thrFactor)
    poke(dut.io.peakGrouping, 0)
    
    val cfarModeNum = cfarMode match {
                        case "Cell Averaging" => 0
                        case "Greatest Of" => 1
                        case "Smallest Of" => 2
                        case _ => 0
                      }
    val cfarAlgorithmTmp = cfarAlgorithm match {
                         case "CA" => 0
                         case "GOS" => 1
                        }
    
    poke(dut.io.cfarMode, cfarModeNum)
    
    if (dut.params.logOrLinReg) {
      poke(dut.io.logOrLinearMode.get, 1)
    }
    
    var lWinSizes: Seq[Int] = Seq()
    if (runTime)
      lWinSizes = CFARUtils.pow2Divisors(dut.params.leadLaggWindowSize).filter(_ > 4).toSeq
    else
      lWinSizes = Seq(dut.params.leadLaggWindowSize)
    val startGwin: Int = if (runTime) 2 else dut.params.guardWindowSize
    // val startLwin: Int = if (runTime) 4 else dut.params.leadLaggWindowSize
    
    //for (lWinSize <- startLwin to dut.params.leadLaggWindowSize) {
    for (lWinSize <- lWinSizes) {
      for (guardSize <- startGwin to dut.params.guardWindowSize) {
        // println(s"Testing CFAR core with lWinSize = $lWinSize and guardSize = $guardSize")
        
        indexLeadTmp = if (indexLead > lWinSize) lWinSize else indexLead
        indexLaggTmp = if (indexLagg > lWinSize) lWinSize else indexLagg
        
        val (expThr, expPeaks) = if (cfarAlgorithm == "GOS")
                                   CFARUtils.cfarGOS(in, referenceCells = lWinSize, guardCells = guardSize, indexLagg = indexLaggTmp, indexLead = indexLeadTmp, cfarMode = cfarMode, considerEdges = considerEdges, scalingFactor = thrFactor, plotEn = thrPlot)
                                 else
                                   CFARUtils.cfarCA(in, cfarMode = cfarMode, referenceCells = lWinSize, guardCells = guardSize, considerEdges = considerEdges, scalingFactor = thrFactor, plotEn = thrPlot)

        poke(dut.io.guardCells, guardSize)
        poke(dut.io.windowCells, lWinSize)

        if (dut.params.CFARAlgorithm != GOSCFARType) {
          poke(dut.io.divSum.get, log2Ceil(lWinSize))
        }

        if (dut.params.CFARAlgorithm == GOSCACFARType) {
          poke(dut.io.cfarAlgorithm.get, cfarAlgorithmTmp)
        }

        poke(dut.io.indexLead.get, indexLeadTmp)
        poke(dut.io.indexLagg.get, indexLaggTmp)
      
        step(2) // be sure that control registers are first initilized and then set ready and valid signals
        poke(dut.io.out.ready, 1)
        
        for (i <- 0 until in.size) {
          poke(dut.io.in.valid, 0)
          val delay = 3//Random.nextInt(5)
          //step(delay)
          for (i <- 0 until delay) {
            if (peek(dut.io.out.valid) == true) {
              dut.params.protoIn match {
                case dspR: DspReal => {
                  realTolDecPts.withValue(tol) { expect(dut.io.out.bits.cut.get,  in(cntValidOut).toDouble) }
                  realTolDecPts.withValue(tol) { expect(dut.io.out.bits.threshold, expThr(cntValidOut)) }
                }
                case _ =>  {
                  fixTolLSBs.withValue(tol) { expect(dut.io.out.bits.cut.get, in(cntValidOut)) }
                  fixTolLSBs.withValue(tol) { expect(dut.io.out.bits.threshold, expThr(cntValidOut)) }
                }
              }
              //fftBin
              /*if (expPeaks.contains(peek(dut.io.fftBin))) {
                expect(dut.io.out.bits.peak, 1)
              }*/
              cntValidOut += 1
              threshold += peek(dut.io.out.bits.threshold)
            }
            step(1)
          }
          poke(dut.io.in.valid, 1)
          poke(dut.io.in.bits, in(i))
          if (i == (in.size - 1))
            poke(dut.io.lastIn, 1)
          if (peek(dut.io.out.valid) == true) {
            dut.params.protoIn match {
              case dspR: DspReal => {
                realTolDecPts.withValue(tol) { expect(dut.io.out.bits.cut.get,  in(cntValidOut).toDouble) }
                realTolDecPts.withValue(tol) { expect(dut.io.out.bits.threshold, expThr(cntValidOut)) }
              }
              case _ =>  {
                fixTolLSBs.withValue(tol) { expect(dut.io.out.bits.cut.get, in(cntValidOut)) }
                fixTolLSBs.withValue(tol) { expect(dut.io.out.bits.threshold, expThr(cntValidOut)) }
              }
            }
            //fftBin
            /*if (expPeaks.contains(peek(dut.io.fftBin))) {
              expect(dut.io.out.bits.peak, 1)
            }*/
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
            /*if (expPeaks.contains(peek(dut.io.fftBin))) {
              expect(dut.io.out.bits.peak, 1)
            }*/
            threshold += peek(dut.io.out.bits.threshold)

            if (cntValidOut == in.size - 1)
              expect(dut.io.lastOut, 1)
            cntValidOut += 1
          }
          step(1)
      }

      cntValidOut = 0
      step(dut.params.leadLaggWindowSize * 2)
      }
    }
  }
  if (thrPlot) {
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

    f.saveas(s"test_run_dir/CFARThresholdGOSCAPlot.pdf")
  }
}

object CFARGOSCATester {
  def apply[T <: Data : Real: BinaryRepresentation](params: CFARParams[T],
                                                    cfarMode: String = "Smallest Of",
                                                    cfarAlgorithm: String = "GOS",
                                                    thrFactor: Double = 3.5,
                                                    considerEdges: Boolean = false,
                                                    indexLagg: Int = 4,
                                                    indexLead: Int = 4,
                                                    random: Boolean = false,
                                                    thrPlot: Boolean = false,
                                                    runTime: Boolean = false,
                                                    backend: String = "verilator",
                                                    tol: Int = 3): Boolean = {
    require(params.fftSize > 8)
    require(indexLagg <= params.leadLaggWindowSize)
    require(indexLead <= params.leadLaggWindowSize)
    require(cfarAlgorithm == "GOS" | cfarAlgorithm == "CA")
    
    var testSignal: Seq[Double] = Seq()
    
    if (random == false) {
      val numSamples = params.fftSize
      Random.setSeed(11110L)
    
      val noise = (0 until numSamples).map(i => Complex(math.sqrt(Random.nextDouble + Random.nextDouble),0))
      val s1    = (0 until numSamples).map(i => Complex(0.4 * math.cos(2 * math.Pi * 1/8 * i), 0.4 * math.sin(2 * math.Pi * 1/8 * i)))
      val s2    = (0 until numSamples).map(i => Complex(0.2 * math.cos(2 * math.Pi * 1/4 * i), 0.2 * math.sin(2 * math.Pi * 1/4 * i)))
      val s3    = (0 until numSamples).map(i => Complex(0.1 * math.cos(2 * math.Pi * 1/2 * i), 0.1 * math.sin(2 * math.Pi * 1/2 * i)))

      // can be simplified
      var sum   = noise.zip(s1).map { case (a, b) => a + b}.zip(s2).map{ case (c, d) => c + d }.zip(s3).map{ case (e, f)  => e + f }
      val fft = fourierTr(DenseVector(sum.toArray)).toScalaVector
      testSignal = fft.map(c => math.sqrt(pow(c.real,2) + pow(c.imag,2)))
    }
    else {
      val testSignaltmp = params.protoIn match {
        //case f: FixedPoint => Seq.fill(params.fftSize)((Random.nextDouble() * 2 - 1) * ((1 << params.protoIn.getWidth - f.binaryPoint.get - 1)))
        // Only positive
//         case f: FixedPoint => Seq.fill(params.fftSize)((Random.nextDouble()) * ((1 << params.protoIn.getWidth - f.binaryPoint.get - 1)))
        case f: FixedPoint => (0 until params.fftSize).map(c => c.toDouble)
        case s: SInt => Seq.fill(params.fftSize)((Random.nextInt((1 << (params.protoIn.getWidth - 1)) * 2) - (1 << (params.protoIn.getWidth - 1))).toDouble)
        case u: UInt => Seq.fill(params.fftSize)(Random.nextInt(1 << (params.protoIn.getWidth)).toDouble)
        case d: DspReal => Seq.fill(params.fftSize)((Random.nextInt(Double.MaxValue.toInt) - Double.MaxValue.toInt).toDouble)
      }
      testSignal = testSignaltmp
    }

    chisel3.iotesters.Driver.execute(Array("-tbn", backend),
      () => new CFARCore(params)) { c => new CFARGOSCATester(c, testSignal,  thrFactor = thrFactor, cfarMode = cfarMode, cfarAlgorithm = cfarAlgorithm,  thrPlot = thrPlot, indexLagg = indexLagg, indexLead = indexLead, considerEdges = considerEdges, runTime = runTime, tol = tol)
    }
  }
}
