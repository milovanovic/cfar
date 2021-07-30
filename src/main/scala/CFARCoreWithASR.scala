package cfar

import chisel3._
import chisel3.experimental._
import chisel3.util._

import dsptools._
import dsptools.numbers._

import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

// Implements CFAR core that can support cell averaging (CA), greatest of (GO), smallest of CFAR (SO)
// Leading/lagging windows are implemented via shift-registers

class CFARCoreWithASR[T <: Data : Real : BinaryRepresentation](val params: CFARParams[T]) extends Module {
  require(params.CFARAlgorithm == CACFARType)
  require(params.includeCASH == true)

  val io = IO(CFARIO(params))
  assert(io.windowCells >= io.subCells.get, "Number of window cells must be larger than number of sub cells")

  val lastCut = RegInit(false.B)
  val flushing = RegInit(false.B)
  val cntIn = RegInit(0.U(log2Ceil(params.fftSize).W))
  val cntOut = RegInit(0.U(log2Ceil(params.fftSize).W))
  val initialInDone = RegInit(false.B)
  val thresholdPipConfig = params.numAddPipes.max(params.numMulPipes) // because of logMode
  val thresholdPip = if (params.logOrLinReg) thresholdPipConfig else if (params.logMode) params.numAddPipes else params.numMulPipes
  
  //val sumPip = if (params.CFARAlgorithm != GOSCFARType) 2 * params.numAddPipes else 0
  //val latencyComp = params.leadLaggWindowSize + params.guardWindowSize + 1 + sumPip + thresholdPip

  val retiming = if (params.retiming) 4 else 0
  val totalDelay = if (params.retiming) retiming + thresholdPip else thresholdPip

  val latencyComp = params.leadLaggWindowSize + params.guardWindowSize + 1 + thresholdPip
  val latency = io.windowCells +& io.guardCells +& 1.U // +& thresholdPip.U
  assert(io.fftWin > 2.U * io.windowCells + 2.U * io.guardCells + 1.U, "FFT size must be larger than total number of shifting cells inside CFAR core")
  val sumT: T = (io.in.bits * log2Ceil(params.leadLaggWindowSize)).cloneType // to prevent overflow
  // val sumlagg = RegInit(t = sumT, init = 0.U.asTypeOf(sumT))
  // val sumlead = RegInit(t = sumT, init = 0.U.asTypeOf(sumT))
  
  val laggWindow    =  Module(new AdjustableShiftRegisterStream(params.protoIn, params.leadLaggWindowSize, sendCnt = true))
  laggWindow.io.in <> io.in
  laggWindow.io.depth := io.windowCells
  laggWindow.io.lastIn := io.lastIn
  val laggOutputVector = laggWindow.io.parallelOut

  val numSums = params.leadLaggWindowSize / params.minSubWindowSize.get
  // println("Number of sums is:")
  // println(numSums)

  val rstProto = Wire(params.protoIn.cloneType)
  rstProto := Real[T].fromDouble(0.0)
  // define vector of registers
  // println(CFARUtils.pow2Divisors(params.fftSize))

  val laggGuard     = Module(new AdjustableShiftRegisterStream(params.protoIn.cloneType, params.guardWindowSize))
  laggGuard.io.in <> laggWindow.io.out
  laggGuard.io.lastIn := laggWindow.io.lastOut
  laggGuard.io.depth  := io.guardCells

  val cellUnderTest = Module(new CellUnderTest(params.protoIn.cloneType))
  cellUnderTest.io.in <> laggGuard.io.out
  cellUnderTest.io.lastIn := laggGuard.io.lastOut
  val lastOut = ShiftRegister(cellUnderTest.io.lastOut, totalDelay, true.B)

  val leadGuard = Module(new AdjustableShiftRegisterStream(params.protoIn.cloneType, params.guardWindowSize))
  leadGuard.io.in <> cellUnderTest.io.out
  leadGuard.io.depth := io.guardCells
  leadGuard.io.lastIn := cellUnderTest.io.lastOut
  leadGuard.io.out.ready := io.out.ready
  
  val leadWindow = Module(new AdjustableShiftRegisterStream(params.protoIn, params.leadLaggWindowSize, sendCnt = true, enInitStore = false))
  leadWindow.io.depth := io.windowCells
  leadWindow.io.in <> leadGuard.io.out
  
  ////////////////////// CHANGED ////////////////////////
  leadWindow.io.out.ready := io.out.ready
  // leadWindow.io.out.ready := Mux(lastCut, true.B, io.out.ready)
  val leadOutputVector = leadWindow.io.parallelOut

  cellUnderTest.io.out.ready := Mux(leadWindow.io.regFull, leadWindow.io.in.ready, io.out.ready)
  leadWindow.io.lastIn := leadGuard.io.lastOut

  val sumSubLaggs = RegInit(VecInit(Seq.fill(numSums)(0.U.asTypeOf(sumT))))
  val sumSubLeads = RegInit(VecInit(Seq.fill(numSums)(0.U.asTypeOf(sumT))))
  val activeSums = WireDefault(VecInit(Seq.fill(numSums)(true.B)))
  
  val maybeFullLagg = WireDefault(VecInit(Seq.fill(numSums)(false.B)))
  val maybeFullLead = WireDefault(VecInit(Seq.fill(numSums)(false.B)))
  
  (0 until numSums).map { numSumIndex => {
    val startIndex = numSumIndex * params.minSubWindowSize.get - 1
    val pow2Divisors = if (numSumIndex == 0) CFARUtils.pow2Divisors(params.leadLaggWindowSize).filter(_ >= params.minSubWindowSize.get) else CFARUtils.pow2Divisors(startIndex + 1).filter(_ >= params.minSubWindowSize.get)

    //println("Pow 2 divisors")
    //pow2Divisors.foreach(c => println(c))
    //println()

    val endIndices = pow2Divisors.map(pow2Divisor => pow2Divisor + startIndex)
    // val endIndex = if (numSumIndex == 0) io.subCells.get else startIndex.U + io.subCells.get  //if (numSumIndex == 0) io.subCells.get - 1.U else startIndex.U + io.subCells.get
    val endIndex = if (numSumIndex == 0) io.subCells.get else startIndex.U + io.subCells.get + 1.U

    dontTouch(endIndex)
    endIndex.suggestName("endIndex")

    val bools = pow2Divisors.map(divisor => divisor.U === (io.subCells.get))
    // this logic for sure can be simplified!
    activeSums(numSumIndex) := bools.foldLeft(false.B)(_ || _) && endIndex <= io.windowCells
    //activeSums(numSumIndex) := bools.foldLeft(false.B)(_ || _)

    dontTouch(activeSums)
    activeSums.suggestName("activeSums")

    val collectedDataLagg = laggOutputVector.zipWithIndex.filter {
      case (datum, index) => endIndices.contains(index) }.map(_._1)
    val collectedDataLead = leadOutputVector.zipWithIndex.filter {
      case (datum, index) => endIndices.contains(index) }.map(_._1)
      
    val casesLagg = bools.zip(collectedDataLagg).map { case (bool, datum) => bool -> datum }
    val casesLead = bools.zip(collectedDataLead).map { case (bool, datum) => bool -> datum }

    val minusOpLagg = MuxCase(rstProto, casesLagg)
    val minusOpLead = MuxCase(rstProto, casesLead)
    dontTouch(minusOpLagg)
    minusOpLagg.suggestName("minusOperandLagg")
    minusOpLead.suggestName("minusOperandLead")
    
    //val maybeFullLead = leadWindow.io.cnt.get > (endIndex - 1.U)
    //val maybeFullLagg = laggWindow.io.cnt.get > (endIndex - 1.U) // if (numSums == 0) laggWindow.cntIn > (endIndex - 1.U) else laggWindow.cntIn > endIndex
    maybeFullLead(numSumIndex) := leadWindow.io.cnt.get > (endIndex - 1.U)
    maybeFullLagg(numSumIndex) := laggWindow.io.cnt.get > (endIndex - 1.U)
    
    dontTouch(maybeFullLagg(numSumIndex))
    maybeFullLagg(numSumIndex).suggestName("maybeFullLagg")
    dontTouch(maybeFullLead(numSumIndex))
    maybeFullLead(numSumIndex).suggestName("maybeFullLead")
    
    dontTouch(sumSubLaggs(numSumIndex))
    sumSubLaggs(numSumIndex).suggestName("sumSubLagg")
    
    when (io.lastOut) {
      sumSubLaggs(numSumIndex) := 0.U.asTypeOf(sumT)
    }
    .elsewhen (io.in.fire()) {
      when (laggWindow.io.regFull || maybeFullLagg(numSumIndex)) {
        if (numSumIndex == 0) {
          sumSubLaggs(numSumIndex) := sumSubLaggs(numSumIndex) + laggWindow.io.in.bits - minusOpLagg
        }
        else {
          sumSubLaggs(numSumIndex) := sumSubLaggs(numSumIndex) + laggOutputVector(startIndex.U) - minusOpLagg
        }
      }
      .otherwise {
        if (numSumIndex == 0) {
          sumSubLaggs(numSumIndex) := sumSubLaggs(numSumIndex) + laggWindow.io.in.bits
        }
        else {
          when (maybeFullLagg(numSumIndex-1)) {
            sumSubLaggs(numSumIndex) := sumSubLaggs(numSumIndex) + laggOutputVector(startIndex.U)
          }
        }
      }
    }

    when (io.lastOut) {
      sumSubLeads(numSumIndex) := 0.U.asTypeOf(sumT)
    }
    .elsewhen (leadWindow.io.in.fire() && cellUnderTest.io.out.fire()) {
    //.elsewhen (leadWindow.io.in.fire()) {
      when (leadWindow.io.regFull || maybeFullLead(numSumIndex)) {
        if (numSumIndex == 0) {
          sumSubLeads(numSumIndex) := sumSubLeads(numSumIndex) + leadWindow.io.in.bits - minusOpLead
        }
        else {
          sumSubLeads(numSumIndex) := sumSubLeads(numSumIndex) + leadOutputVector(startIndex.U) - minusOpLead
        }
      }
      .otherwise {
        if (numSumIndex == 0) {
          sumSubLeads(numSumIndex) := sumSubLeads(numSumIndex) + leadWindow.io.in.bits
        }
        else {
          when (maybeFullLead(numSumIndex - 1)) {
            sumSubLeads(numSumIndex) := sumSubLeads(numSumIndex) + leadOutputVector(startIndex.U)
          }
        }
      }
    }
  }
  }
  val diffInSubSize = Log2(io.subCells.get) - Log2(params.minSubWindowSize.get.U)
  val max = sumSubLaggs.zip(sumSubLeads).map { case (lagg, lead) => Real[T].max(lagg, lead) }
  val maxFinal = if (params.retiming) RegInit(VecInit(Seq.fill(numSums)(0.U.asTypeOf(sumT))))
  else WireDefault(VecInit(Seq.fill(numSums)(0.U.asTypeOf(sumT))))
  
  max.zip(activeSums).zipWithIndex.foreach {
    case ((max, active), ind) => {
      when (active === true.B) {
        val address = ind.U >> diffInSubSize
        maxFinal(address) := max
      }
    }
  }
  
  val minCircuit = Module(new MinimumCircuit(sumT.cloneType, numSums, runTime = true, retiming = true))
  minCircuit.io.in := maxFinal
  minCircuit.io.inSize.get := io.windowCells >> Log2(io.subCells.get) //io.windowCells
  val min = if (params.retiming) RegNext(minCircuit.io.out >> Log2(io.subCells.get)) else minCircuit.io.out >> Log2(io.subCells.get)
  
  dontTouch(min)
  min.suggestName("clutterRepr")

  when (io.in.fire()) {
    cntIn := cntIn + 1.U
  }
  when (cntIn === (latency - 1.U) && io.in.fire()) {
    initialInDone := true.B
  }
 
  when (io.lastOut && io.out.fire()) {
    cntIn := 0.U
  }
  when (io.out.fire()) {
    cntOut := cntOut + 1.U
  }
  when (cntOut === (io.fftWin - 1.U) && io.out.fire() || (io.lastOut && io.out.fire)) {
    cntOut := 0.U
  }

  when (io.lastIn) {
    flushing := true.B
  }

  val flushingDelayed =  ShiftRegisterWithReset(in = flushing,
                                              n = totalDelay,
                                              resetData = false.B,
                                              reset = io.lastOut,
                                              en = true.B)

  when (lastOut) {
    lastCut := true.B
    flushing := false.B
    initialInDone := false.B
  }
  when (leadWindow.io.regEmpty === true.B) {
    lastCut := false.B
  }

//  if it is necessary to provide at the same time result of the CA, SO, GO and CASH algorithm then this part should be uncommented
//   when (io.lastOut) {
//     sumlagg := 0.U.asTypeOf(sumT)
//   }
//   .elsewhen (io.in.fire()) {
//     when (laggWindow.io.regFull) {
//       when (laggWindow.io.out.fire()) {
//         sumlagg := sumlagg + laggWindow.io.in.bits - laggWindow.io.out.bits
//       }
//     }
//     .otherwise {
//       sumlagg := sumlagg + laggWindow.io.in.bits
//     }
//   }
//   dontTouch(sumlagg)
//  
//   when (lastCut) {
//     sumlead := 0.U.asTypeOf(sumT)
//   }
//   .elsewhen (leadWindow.io.in.fire()) {
//     when (leadWindow.io.regFull) {
//       when (leadWindow.io.out.fire()) {
//         sumlead := sumlead + leadWindow.io.in.bits - leadWindow.io.out.bits
//       }
//     }
//     .otherwise {
//       sumlead := sumlead + leadWindow.io.in.bits
//     }
//   }
//   dontTouch(sumlead)

//  val leftThr    = BinaryRepresentation[T].shr(sumlagg, io.divSum.get)
//  val rightThr   = BinaryRepresentation[T].shr(sumlead, io.divSum.get)
  
  val leftThr    = if (params.retiming) ShiftRegister(BinaryRepresentation[T].shr(sumSubLaggs(0), io.divSum.get), 4, en = true.B) else BinaryRepresentation[T].shr(sumSubLaggs(0), io.divSum.get)// BinaryRepresentation[T].shr(sumSubLaggs(0), io.divSum.get)
  val rightThr   = if (params.retiming) ShiftRegister(BinaryRepresentation[T].shr(sumSubLeads(0), io.divSum.get), 4, en = true.B) else BinaryRepresentation[T].shr(sumSubLeads(0), io.divSum.get)
  //BinaryRepresentation[T].shr(sumSubLeads(0), io.divSum.get)
  
  val greatestOf = Mux(leftThr > rightThr, leftThr, rightThr)
  val smallestOf = Mux(leftThr < rightThr, leftThr, rightThr)

  // added CASH mode
  val thrByModes = MuxLookup(io.cfarMode, smallestOf, Array(
                              0.U -> BinaryRepresentation[T].shr(rightThr + leftThr, 1),
                              1.U -> greatestOf,
                              2.U -> smallestOf,
                              3.U -> min))

  val enableRightThr = RegInit(false.B)
  when (!(laggWindow.io.regFull) && laggWindow.io.out.fire()) {
    enableRightThr := true.B
  }
  when (io.lastOut) {
    enableRightThr := false.B
  }

  val thrWithoutScaling = params.edgesMode match {
    case Cyclic  => if (params.retiming)
                      RegNext(Mux(ShiftRegister(laggWindow.io.regFull && leadWindow.io.regFull, 3, en = true.B),
                        //thrByModes, Mux(ShiftRegister(enableRightThr || !leadWindow.io.regFull, 3, en = true.B), 0.U.asTypeOf(sumT), thrByModes)))
                        thrByModes, Mux(ShiftRegister(enableRightThr || !leadWindow.io.regFull, 3, en = true.B), Mux(enableRightThr, rightThr, leftThr), thrByModes)))
                    else
                       Mux(laggWindow.io.regFull && leadWindow.io.regFull,
                         //thrByModes, Mux(enableRightThr || !leadWindow.io.regFull, 0.U.asTypeOf(sumT), thrByModes))
                         thrByModes, Mux(enableRightThr || !leadWindow.io.regFull, Mux(enableRightThr, rightThr, leftThr), thrByModes))
    case _  =>
      val leftFlag  = WireDefault(false.B)
      val rightFlag = WireDefault(false.B)
      val cntCutOut = RegInit(0.U(log2Ceil(params.fftSize).W))

      when (cellUnderTest.io.out.fire()) {
        cntCutOut := cntCutOut + 1.U
      }
      when (cntCutOut === (io.fftWin - 1.U) && cellUnderTest.io.out.fire()) {
        cntCutOut := 0.U
      }

      when (cntCutOut < (io.windowCells + io.guardCells)) {
        leftFlag := true.B
      }
      .otherwise {
        leftFlag := false.B
      }

      when (cntCutOut > (io.fftWin - io.windowCells - io.guardCells - 1.U)) {
        rightFlag := true.B
      }
      .otherwise {
        rightFlag := false.B
      }

      val leftFlagShift = ShiftRegister(leftFlag, 3, en = true.B)
      val rightFlagShift = ShiftRegister(rightFlag, 3, en = true.B)

      // Make those as an option and try to generalize this parameter retiming!!!
      val zeroThrNoScaling = if (params.retiming)
                               RegNext(Mux(leftFlagShift || rightFlagShift, 0.U.asTypeOf(sumT), thrByModes))
                             else
                               Mux(leftFlag || rightFlag, 0.U.asTypeOf(sumT), thrByModes)

      val cyclicThrNoScaling = if (params.retiming)
                                RegNext(Mux(ShiftRegister(laggWindow.io.regFull && leadWindow.io.regFull, 3, en = true.B),
                                  //thrByModes, Mux(ShiftRegister(enableRightThr || !leadWindow.io.regFull, 3, en = true.B), 0.U.asTypeOf(sumT), thrByModes)))
                                  thrByModes, Mux(ShiftRegister(enableRightThr || !leadWindow.io.regFull, 3, en = true.B), leftThr, thrByModes)))
                               else
                                Mux(laggWindow.io.regFull && leadWindow.io.regFull,
                                  //thrByModes, Mux(enableRightThr || !leadWindow.io.regFull, 0.U.asTypeOf(sumT), thrByModes))
                                  thrByModes, Mux(enableRightThr || !leadWindow.io.regFull, leftThr, thrByModes))


      val nonCyclicThrNoScaling = if (params.retiming)
                                    RegNext(Mux(leftFlagShift, leftThr, Mux(rightFlagShift, rightThr, thrByModes)))
                                  else
                                    Mux(leftFlag, leftThr, Mux(rightFlag, rightThr, thrByModes))

      if (params.edgesMode == AllEdgeModes) {
        Mux(io.edgesMode.get === 0.U, zeroThrNoScaling, Mux(io.edgesMode.get === 1.U, nonCyclicThrNoScaling, cyclicThrNoScaling))
      }
      else if (params.edgesMode == NonCyclic) {
        nonCyclicThrNoScaling
      }
      else {
        zeroThrNoScaling
      }
  }

  val threshold = if (params.logOrLinReg) DspContext.alter(DspContext.current.copy(
    numAddPipes = thresholdPip,
    numMulPipes = thresholdPip)) {
      Mux(io.logOrLinearMode.get, thrWithoutScaling context_* io.thresholdScaler, thrWithoutScaling context_+ io.thresholdScaler)
    }
    else if (!params.logMode)
      DspContext.withNumMulPipes(params.numMulPipes) {
        thrWithoutScaling context_* io.thresholdScaler
      }
    else
      DspContext.withNumAddPipes(params.numAddPipes) {
        thrWithoutScaling context_+ io.thresholdScaler
      }
  
  val cutDelayed = ShiftRegister(cellUnderTest.io.out.bits, totalDelay, en = true.B)
  //ShiftRegister(cellUnderTest.io.out.bits, thresholdPip, en = cellUnderTest.io.out.fire() || (flushing && io.out.ready))
  
  val leftNeighb  = ShiftRegister(Mux(io.guardCells === 0.U, laggWindow.io.parallelOut(io.windowCells - 1.U), laggGuard.io.parallelOut(io.guardCells - 1.U)), totalDelay, en = true.B)
  val rightNeighb = ShiftRegister(Mux(io.guardCells === 0.U, leadWindow.io.parallelOut.head, leadGuard.io.parallelOut.head), totalDelay, en = true.B)
  val isLocalMax = cutDelayed > leftNeighb && cutDelayed > rightNeighb
  val isPeak = cutDelayed  > threshold


  if (params.numAddPipes == 0 && params.numMulPipes == 0) {
    if (params.edgesMode == Zero) {
      val zeroFlag = Mux(cntOut < (io.windowCells + io.guardCells) || cntOut > (io.fftWin - io.windowCells - io.guardCells - 1.U), true.B, false.B)
      io.out.bits.peak := Mux(zeroFlag, false.B, Mux(io.peakGrouping, isPeak && isLocalMax, isPeak))
    }
    else {
      io.out.bits.peak := Mux(io.peakGrouping, isPeak && isLocalMax, isPeak)
    }
    io.out.bits.threshold := threshold
    if (params.sendCut)
      io.out.bits.cut.get := cutDelayed
    io.in.ready := ~initialInDone || io.out.ready && ~flushing
    io.out.valid := initialInDone && io.in.fire() || flushing
    io.lastOut := lastOut
    io.fftBin := cntOut
  }
  else {
    val queueData = Module(new Queue((io.out.bits.cloneType), totalDelay + 1, flow = true))
    io.in.ready := ~initialInDone || io.out.ready && ~flushingDelayed
    queueData.io.enq.valid := ShiftRegister(initialInDone && io.in.fire(), totalDelay, en = true.B) || (flushingDelayed && ShiftRegister(io.out.ready, totalDelay, en = true.B))

    if (params.sendCut)
      queueData.io.enq.bits.cut.get := cutDelayed
    queueData.io.enq.bits.threshold := threshold

    if (params.edgesMode == Zero) {
      val zeroFlag = Mux(cntOut < (io.windowCells + io.guardCells) || cntOut > (io.fftWin - io.windowCells - io.guardCells - 1.U), true.B, false.B)
      queueData.io.enq.bits.peak := Mux(zeroFlag, false.B, Mux(io.peakGrouping, isPeak && isLocalMax, isPeak))
    }
    else {
      queueData.io.enq.bits.peak := Mux(io.peakGrouping, isPeak && isLocalMax, isPeak)
    }
    queueData.io.deq.ready := io.out.ready

    val queueLast = Module(new Queue(Bool(), totalDelay + 1, flow = true))
    queueLast.io.enq.valid := ShiftRegister(initialInDone && io.in.fire(), totalDelay, en = true.B) || (flushingDelayed && ShiftRegister(io.out.ready, totalDelay, en = true.B))
    queueLast.io.enq.bits := lastOut

    queueLast.io.deq.ready := io.out.ready
    io.lastOut := queueLast.io.deq.bits

    if (params.sendCut)
      io.out.bits.cut.get := queueData.io.deq.bits.cut.get
    io.out.bits.peak := queueData.io.deq.bits.peak
    io.out.bits.threshold := queueData.io.deq.bits.threshold

    io.out.valid := queueData.io.deq.valid
    io.fftBin := cntOut
  }
}

object CFARCoreWithASREApp extends App
{
  // just simple test
  val params: CFARParams[FixedPoint] =  CFARParams(
    protoIn = FixedPoint(24.W, 8.BP),
    protoThreshold = FixedPoint(16.W, 8.BP), // output thres
    protoScaler = FixedPoint(16.W, 8.BP),
    CFARAlgorithm = CACFARType,
    fftSize = 1024,
    leadLaggWindowSize = 128,
    guardWindowSize = 4,
    includeCASH = true,
    minSubWindowSize = Some(8),
    edgesMode = Cyclic
   // other parameters are default
  )
  
  chisel3.Driver.execute(args,()=>new CFARCoreWithASR(params))
}
