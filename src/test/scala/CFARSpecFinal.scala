package cfar

import chisel3._
import chisel3.util._
import chisel3.experimental._

import dsptools._
import dsptools.numbers._

import org.scalatest.{FlatSpec, Matchers}

class CFARFinalSpec extends FlatSpec with Matchers {

  val fftSize = 512
  val thrPlot = false
  val considerEdges = false
  val thrFactor = 3.5
  val runTime = true
  val random = false

  // Important: when retiming is on then numMulPipes needs to be equal to 1
  // There are some issues when verilator is used and large amount of tests are run. When treadle is activated then everything passes.
  // Another important note is that if we want to suport not only power of 2 windowCell size then minSubWindowSize needs to be 2 and that adds aditional logic into the system - not tested for that case on hardware

  for (cfarMode <- Seq("CASH", "Greatest Of", "Smallest Of", "Cell Averaging")) {
    for (refWindow <- Seq(16, 32)) { // 32, 64, 128
      for (guardWindow <- Seq(2)) { //, 4, 8)) {
        for (backend <- Seq("verilator")) {//, "treadle")) {
          if (cfarMode == "CASH") {
            for (subWindowSize <- CFARUtils.pow2Divisors(refWindow).tail.toSeq) { //) { 
              println(subWindowSize.toString)
              val paramsFixedASR: CFARParams[FixedPoint] = CFARParams(
                protoIn = FixedPoint(16.W, 6.BP),
                protoThreshold = FixedPoint(16.W, 6.BP),
                protoScaler = FixedPoint(16.W, 6.BP),
                leadLaggWindowSize = refWindow,
                guardWindowSize = guardWindow,
                fftSize = fftSize,
                minSubWindowSize = Some(subWindowSize),
                includeCASH = true,
                CFARAlgorithm = CACFARType
              )
              it should s"test CA/SO/GO/CASH-CFAR core with reference window = $refWindow, guard window = $guardWindow, cfarMode = $cfarMode and backend = $backend and subWindowSize = $subWindowSize" in {
                CFARCATester(paramsFixedASR,
                            cfarMode = cfarMode,
                            thrFactor = thrFactor,
                            considerEdges = false,
                            runTime = runTime,
                            random = random,
                            backend = backend,
                            tol = 3) should be (true)
              }
            }
          }
          else {
            val paramsFixedASR: CFARParams[FixedPoint] = CFARParams(
              protoIn = FixedPoint(16.W, 6.BP),
              protoThreshold = FixedPoint(16.W, 6.BP),
              protoScaler = FixedPoint(16.W, 6.BP),
              leadLaggWindowSize = refWindow,
              guardWindowSize = guardWindow,
              fftSize = fftSize,
              minSubWindowSize = Some(2), // test CACFAR with Adjustable shift registers
              includeCASH = true,
              CFARAlgorithm = CACFARType
            )
            it should s"test CA/SO/GO/CASH-CFAR core with reference window = $refWindow, guard window = $guardWindow, cfarMode = $cfarMode and backend = $backend" in {
            CFARCATester(paramsFixedASR,
                        cfarMode = cfarMode,
                        thrFactor = thrFactor,
                        considerEdges = considerEdges,
                        runTime = runTime,
                        random = random,
                        backend = backend,
                        tol = 3) should be (true)
            }
          }
        }
      }
    }
  }
  
  // Test module CFARCoreWithASR, FixedPoint data type and included pipes and retiming
  
  for (cfarMode <- Seq("CASH", "Greatest Of", "Smallest Of", "Cell Averaging")) {//, "Greatest Of", "Smallest Of", "Cell Averaging")) {
    for (refWindow <- Seq(16, 32)) {
      for (guardWindow <- Seq(2)) { //, 4, 8)) {
        for (backend <- Seq("treadle")) {//, "treadle")) {
          if (cfarMode == "CASH") {
            for (subWindowSize <- CFARUtils.pow2Divisors(refWindow).tail.toSeq) { //) { 
              println(subWindowSize.toString)
              val paramsFixedASR: CFARParams[FixedPoint] = CFARParams(
                protoIn = FixedPoint(16.W, 6.BP),
                protoThreshold = FixedPoint(16.W, 6.BP),
                protoScaler = FixedPoint(16.W, 6.BP),
                leadLaggWindowSize = refWindow,
                guardWindowSize = guardWindow,
                retiming = true,
                numMulPipes = 1,
                fftSize = fftSize,
                minSubWindowSize = Some(subWindowSize),
                includeCASH = true,
                CFARAlgorithm = CACFARType
              )
              it should s"test CA/SO/GO/CASH-CFAR core with reference window = $refWindow, guard window = $guardWindow, cfarMode = $cfarMode and backend = $backend and subWindowSize = $subWindowSize and numMulPipes = 1" ignore {
                CFARCATester(paramsFixedASR,
                            cfarMode = cfarMode,
                            thrFactor = thrFactor,
                            considerEdges = considerEdges,
                            runTime = runTime,
                            random = random,
                            backend = backend,
                            tol = 3) should be (true)
              }
            }
          }
          else {
            val paramsFixedASR: CFARParams[FixedPoint] = CFARParams(
              protoIn = FixedPoint(16.W, 6.BP),
              protoThreshold = FixedPoint(16.W, 6.BP),
              protoScaler = FixedPoint(16.W, 6.BP),
              leadLaggWindowSize = refWindow,
              guardWindowSize = guardWindow,
              retiming = true,
              numMulPipes = 1,
              fftSize = fftSize,
              minSubWindowSize = Some(2), // test CACFAR with Adjustable shift registers
              includeCASH = true,
              CFARAlgorithm = CACFARType
            )
            it should s"test CA/SO/GO/CASH-CFAR core with reference window = $refWindow, guard window = $guardWindow, cfarMode = $cfarMode and backend = $backend and numMulPipes = 1" ignore {
              CFARCATester(paramsFixedASR,
                          cfarMode = cfarMode,
                          thrFactor = thrFactor,
                          considerEdges = considerEdges,
                          runTime = runTime,
                          random = random,
                          backend = backend,
                          tol = 3) should be (true)
            }
          }
        }
      }
    }
  }
  // Test module CFARCoreWithMem and FixedPoint data type
  for (cfarMode <- Seq("Cell Averaging", "Greatest Of", "Smallest Of")) {
    for (refWindow <- Seq(16, 32, 64, 128)) {
      for (guardWindow <- Seq(2, 4, 8)) {
        for (backend <- Seq("verilator", "treadle")) {
          val paramsFixedMem: CFARParams[FixedPoint] = CFARParams(
            protoIn = FixedPoint(16.W, 6.BP),
            protoThreshold = FixedPoint(16.W, 6.BP),
            protoScaler = FixedPoint(16.W, 6.BP),
            leadLaggWindowSize = refWindow,
            guardWindowSize = guardWindow,
            fftSize = fftSize,
            minSubWindowSize = None, // test CACFAR with ShiftRegisterMem -> blockram/sram
            includeCASH = false,
            CFARAlgorithm = CACFARType
          )
          it should s"test CA/SO/GO-CFAR core with reference window = $refWindow, guard window = $guardWindow, cfarMode = $cfarMode and backend = $backend" ignore {
          CFARCATester(paramsFixedMem,
                      cfarMode = cfarMode,
                      thrFactor = thrFactor,
                      considerEdges = considerEdges,
                      runTime = runTime,
                      random = random,
                      backend = backend,
                      tol = 3) should be (true)
          }
        }
      }
    }
  }

  // Test generalized order statistic algorithms!
  for (cfarAlgorithm <- Seq("GOS", "CA")) {
    for (cfarMode <- Seq("Smallest Of", "Greatest Of", "Cell Averaging")) {
      for (refWindow <- Seq(16, 32, 64, 128)) {
        for (guardWindow <- Seq(2, 4, 8)) {
          for (backend <- Seq("verilator", "treadle")) {
            val paramsFixedLis: CFARParams[FixedPoint] = CFARParams(
              protoIn = FixedPoint(16.W, 6.BP),
              protoThreshold = FixedPoint(16.W, 6.BP),
              protoScaler = FixedPoint(16.W, 6.BP),
              leadLaggWindowSize = refWindow,
              guardWindowSize = guardWindow,
              fftSize = fftSize,
              minSubWindowSize = None,
              includeCASH = false,
              CFARAlgorithm = GOSCACFARType
            )
            it should s"test GOSCA CFAR core with reference window = $refWindow, guard window = $guardWindow, cfarMode = $cfarMode , cfarAlgorithm = $cfarAlgorithm and backend = $backend" ignore {
            CFARGOSCATester(paramsFixedLis,
                        cfarMode = cfarMode,
                        cfarAlgorithm = cfarAlgorithm,
                        thrFactor = thrFactor,
                        considerEdges = false,
                        indexLagg = refWindow/2,
                        indexLead = refWindow/2,
                        runTime = runTime, // taking too late to simulate if it is true
                        random = random,
                        backend = backend,
                        tol = 3) should be (true)
            }
          }
        }
      }
    }
  }
  
  // Test generalized order statistic algorithms!
  // Turn on retiming
  for (cfarAlgorithm <- Seq("GOS")) { //, "CA")) {
    for (cfarMode <- Seq("Smallest Of", "Greatest Of", "Cell Averaging")) {
      for (refWindow <- Seq(16)) {//, 32, 64, 128)) {
        for (guardWindow <- Seq(2)) {// 4, 8)) {
          for (backend <- Seq("verilator", "treadle")) {
            val paramsFixedLis: CFARParams[FixedPoint] = CFARParams(
              protoIn = FixedPoint(16.W, 6.BP),
              protoThreshold = FixedPoint(16.W, 6.BP),
              protoScaler = FixedPoint(16.W, 6.BP),
              leadLaggWindowSize = refWindow,
              guardWindowSize = guardWindow,
              retiming = true,
              numMulPipes = 1,
              fftSize = fftSize,
              minSubWindowSize = None,
              includeCASH = false,
              CFARAlgorithm = GOSCACFARType
            )
            it should s"test GOSCA CFAR core with reference window = $refWindow, guard window = $guardWindow, cfarMode = $cfarMode , cfarAlgorithm = $cfarAlgorithm and backend = $backend and numMulPipes = 1" ignore {
            CFARGOSCATester(paramsFixedLis,
                        cfarMode = cfarMode,
                        cfarAlgorithm = cfarAlgorithm,
                        thrFactor = thrFactor,
                        considerEdges = false,
                        indexLagg = refWindow/2,
                        indexLead = refWindow/2,
                        runTime = runTime, // taking too late to simulate if it is true
                        random = random,
                        backend = backend,
                        tol = 3) should be (true)
            }
          }
        }
      }
    }
  }
  
  // Simple test for DspReal data type
  for (cfarAlgorithm <- Seq("GOS", "CA")) {
    for (cfarMode <- Seq("Smallest Of", "Greatest Of", "Cell Averaging")) {
      for (refWindow <- Seq(16)) {
        for (guardWindow <- Seq(2)) {
          for (backend <- Seq("verilator", "treadle")) {
            val paramsFixedLis: CFARParams[DspReal] = CFARParams(
              protoIn = DspReal(),
              protoThreshold = DspReal(),
              protoScaler = DspReal(),
              leadLaggWindowSize = refWindow,
              guardWindowSize = guardWindow,
              fftSize = fftSize,
              minSubWindowSize = None, 
              includeCASH = false,
              CFARAlgorithm = GOSCACFARType
            )
            it should s"test GOSCA CFAR core with reference window = $refWindow, guard window = $guardWindow, cfarMode = $cfarMode , cfarAlgorithm = $cfarAlgorithm and backend = $backend and DspReal data type" ignore {
            CFARGOSCATester(paramsFixedLis,
                        cfarMode = cfarMode,
                        cfarAlgorithm = cfarAlgorithm,
                        thrFactor = thrFactor,
                        considerEdges = false,
                        indexLagg = refWindow/2,
                        indexLead = refWindow/2,
                        runTime = runTime,
                        random = random,
                        backend = backend,
                        tol = 12) should be (true)
            }
          }
        }
      }
    }
  }

  // just test retiming and pipeline registers
  for (cfarMode <- Seq("Cell Averaging", "Greatest Of", "Smallest Of")) {
    for (refWindow <- Seq(16, 32)) { //, 64, 128)) {
      for (guardWindow <- Seq(2)) {//, 4, 8)) {
        for (backend <- Seq("verilator")) {//, "treadle")) {
          for (numPipes <- Seq(1, 2, 3)) {
            val paramsFixedMem: CFARParams[FixedPoint] = CFARParams(
              protoIn = FixedPoint(16.W, 6.BP),
              protoThreshold = FixedPoint(16.W, 6.BP),
              protoScaler = FixedPoint(16.W, 6.BP),
              leadLaggWindowSize = refWindow,
              guardWindowSize = guardWindow,
              retiming = true,
              numMulPipes = numPipes,
              fftSize = fftSize,
              minSubWindowSize = None, // test CACFAR with ShiftRegisterMem -> blockram/sram
              includeCASH = false,
              CFARAlgorithm = CACFARType
            )
          it should s"test CA/SO/GO-CFAR core with reference window = $refWindow, guard window = $guardWindow, cfarMode = $cfarMode and backend = $backend and numMulPipes = $numPipes and retiming included" ignore {
          CFARCATester(paramsFixedMem,
                      cfarMode = cfarMode,
                      thrFactor = thrFactor,
                      considerEdges = considerEdges,
                      runTime = runTime,
                      random = random,
                      backend = backend,
                      tol = 3) should be (true)
            }
          }
        }
      }
    }
  }

// Those test cases are used only for testing new functionalities
/*  for (cfarMode <- Seq("Greatest Of")) {
    for (refWindow <- Seq(16)) { // 32, 64, 128
      for (guardWindow <- Seq(2)) { //, 4, 8)) {
        for (backend <- Seq("verilator")) {//, "treadle")) {
          val paramsFixedASR: CFARParams[FixedPoint] = CFARParams(
            protoIn = FixedPoint(16.W, 6.BP),
            protoThreshold = FixedPoint(16.W, 6.BP),
            protoScaler = FixedPoint(16.W, 6.BP),
            leadLaggWindowSize = refWindow,
            guardWindowSize = guardWindow,
            retiming = true,
            numMulPipes = 1,
            fftSize = fftSize,
            minSubWindowSize = Some(2), // test CACFAR with Adjustable shift registers
            includeCASH = true,
            CFARAlgorithm = CACFARType,
            edgesMode = Cyclic
          )
          it should s"test CA/SO/GO/CASH-CFAR core with reference window = $refWindow, guard window = $guardWindow, cfarMode = $cfarMode and backend = $backend - debug test" in {
          CFARCATester(paramsFixedASR,
                      cfarMode = cfarMode,
                      thrFactor = thrFactor,
                      considerEdges = considerEdges,
                      runTime = false,
                      random = random,
                      backend = backend,
                      tol = 3) should be (true)
          }
        }
      }
    }
  }*/

//    for (cfarMode <- Seq("Cell Averaging")) {
//     for (refWindow <- Seq(16)) {
//       for (guardWindow <- Seq(2)) {
//         for (backend <- Seq("verilator")) {
//           val paramsFixedMem: CFARParams[FixedPoint] = CFARParams(
//             protoIn = FixedPoint(24.W, 12.BP),
//             protoThreshold = FixedPoint(24.W, 12.BP),
//             protoScaler = FixedPoint(24.W, 12.BP),
//             leadLaggWindowSize = refWindow,
//             guardWindowSize = guardWindow,
//             retiming = true,//true,
//             numMulPipes = 3,
//             fftSize = fftSize,
//             minSubWindowSize = None, // test CACFAR with ShiftRegisterMem -> blockram/sram
//             includeCASH = false,
//             CFARAlgorithm = CACFARType
//           )
//           it should s"test CA/SO/GO-CFAR core with reference window = $refWindow, guard window = $guardWindow, cfarMode = $cfarMode and backend = $backend and numMulPipes = 1" in {
//           CFARCATester(paramsFixedMem,
//                       cfarMode = cfarMode,
//                       thrFactor = thrFactor,
//                       considerEdges = considerEdges,
//                       runTime = false,
//                       random = random,
//                       backend = backend,
//                       tol = 3) should be (true)
//           }
//         }
//       }
//     }
//   }
}
