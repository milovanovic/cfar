package cfar

import chisel3._
import chisel3.util._
import chisel3.internal.{requireIsChiselType, requireIsHardware}

import breeze.linalg._
import breeze.plot._

import scala.math

object CFARUtils {
  def factorize(x: Int): List[Int] = {
    def foo(x: Int, a: Int = 2, list: List[Int] = Nil): List[Int] = a * a > x match {
      case false if x % a == 0 => foo(x / a, a, a :: list)
      case false => foo(x, a + 1, list)
      case true => x :: list
    }
    foo(x)
  }

  def pow2Divisors(n: Int): List[Int] = {
    val factors = factorize(n)
    val products = (1 to factors.length).flatMap(i => factors.combinations(i).map(_.product).toList).toList
    (1 +: products).filter(isPow2(_))
  }
  
  // Cell Averaging Statistic  Hofele (CASH) cfar
  def cfarCASH(signal: Seq[Double], referenceCells: Int, subCells: Int,  guardCells: Int = 0, scalingFactor: Double, logMode: Boolean = false, plotEn: Boolean = false): (Seq[Double], Seq[Int]) = {
    require(referenceCells > 0 & subCells > 0, "Number of reference and sub cells must be positive value")
    val totalCells = signal.size
    val numSubs = referenceCells / subCells
    val windowCells = referenceCells + guardCells
    var maximums : Seq[Double] = Seq()
    var threshold : Seq[Double] = Seq()
    var peakIndices: Seq[Int] = Seq()
    var subLeadStart = 0
    var subLeadEnd  = 0
    
    // maybe can be implemented in a more elagant way
    for (ix <- windowCells until totalCells - windowCells) {
      for (subIx <- ((1 until (numSubs + 1)).reverse)) {
        subLeadStart = ix - subIx * subCells - guardCells
        subLeadEnd = ix - (subIx - 1) * subCells - guardCells
        val subLaggStart = ix + guardCells + (numSubs - subIx) * subCells + 1
        val subLaggEnd = ix - guardCells + (numSubs - subIx + 1) * subCells + 1
        val subLeadAvg = signal.slice(subLeadStart, subLeadEnd).sum / subCells
        val subLaggAvg = signal.slice(subLaggStart, subLaggEnd).sum / subCells
//         if (ix == windowCells + 1) {
//           println("Second Sums are:")
//           println(subLeadAvg)
//           println(subLaggAvg)
//         }
        maximums = maximums :+ Seq(subLeadAvg, subLaggAvg).max
      }
     // println(maximums.min)
      val scaledThr = if (logMode)  maximums.min + scalingFactor else  maximums.min * scalingFactor
      maximums = Seq()
      threshold = threshold :+ scaledThr
      if (scaledThr < signal(ix)) peakIndices = peakIndices :+ ix
    }
    val edges = Seq.fill(referenceCells)(0.0) // for edges zero should be sent
    threshold = edges ++ threshold ++ edges
    if (plotEn == true) {
      val f = Figure()
      val p = f.subplot(0)
      p.legend_=(true)
      val xaxis = (0 until signal.size).map(e => e.toDouble).toSeq.toArray
      p.xlabel = "Frequency bin"
      p.ylabel = "Amplitude"

      val thresholdPlot = threshold.toSeq

      p += plot(xaxis, signal.toArray, name = "FFT input Signal")
      p += plot(xaxis, threshold.toArray, name = "CFAR threshold") //'.'
      p.title_=(s"Constant False Alarm Rate CASH")

      f.saveas(s"test_run_dir/CFAR_CASH_Scala_Threshold_Plot.pdf")
    }
    (threshold, peakIndices)
  }
  
  // cell averaging CFAR algorithms
  def cfarCA(signal: Seq[Double], cfarMode: String, referenceCells: Int, guardCells: Int, considerEdges: Boolean = false, scalingFactor: Double, logMode: Boolean = false, plotEn: Boolean = false): (Seq[Double], Seq[Int]) = {
    require(referenceCells > 0 & guardCells > 0, "Number of reference and sub cells must be positive value")
    val totalCells = signal.size
    val windowCells = referenceCells + guardCells
    var threshold : Seq[Double] = Seq()
    var peakIndices: Seq[Int] = Seq()

    if (considerEdges) {
      var thr: Double = 0.0
      // take into account only one reference window for the edge cells
      for (ix <- 0 until totalCells) {
        val leadAvg = if (ix < windowCells) 0 else signal.slice(ix - windowCells, ix - guardCells).sum / referenceCells
        val laggAvg = if (ix >= (totalCells - windowCells)) 0 else signal.slice(ix + guardCells + 1, ix + windowCells + 1).sum / referenceCells
        if (ix < windowCells) {
          thr = laggAvg
        }
        else if (ix >= (totalCells - windowCells)) {
          thr = leadAvg
        }
        else {
          thr = cfarMode match {
                  case "Cell Averaging" => (leadAvg + laggAvg)/2
                  case "Smallest Of" => Seq(leadAvg, laggAvg).min
                  case "Greatest Of" => Seq(leadAvg, laggAvg).max
                  case _ =>  throw new Exception(s"Unknown CFAR type, try with Cell Averaging, Smallest Of or Greatest Of")
                }
        }
        val scaledThr = if (logMode) thr + scalingFactor else thr * scalingFactor
        threshold = threshold :+ scaledThr
        if (scaledThr < signal(ix)) peakIndices :+ ix
      }
    }
    else {
//       val leadAvg = signal.take(totalCells - windowCells).slide(referenceCells)
//                           .map(_.sum).toSeq
//       val laggAvg = signal.drop(windowCells).slide(referenceCells)
//                           .map(_.sum).toSeq
      for (ix <- windowCells until (totalCells - windowCells)) {
        val leadAvg = signal.slice(ix - windowCells, ix - guardCells).sum / referenceCells
        val laggAvg = signal.slice(ix + guardCells + 1, ix + windowCells + 1).sum / referenceCells
        val thr = cfarMode match {
                    case "Cell Averaging" => (leadAvg + laggAvg)/2
                    case "Smallest Of" => Seq(leadAvg, laggAvg).min
                    case "Greatest Of" => Seq(leadAvg, laggAvg).max
                    case _ =>  throw new Exception(s"Unknown CFAR type, try with Cell Averaging, Smallest Of or Greatest Of")
                  }
        val scaledThr = if (logMode) thr + scalingFactor else thr * scalingFactor
        if (scaledThr < signal(ix)) peakIndices = peakIndices :+ ix
        threshold = threshold :+ scaledThr
      }
      val edges: Seq[Double] =  Seq.fill(windowCells)(0.0)
      threshold = edges ++ threshold ++ edges
    }
    
    if (plotEn == true) {
      val f = Figure()
      val p = f.subplot(0)
      p.legend_=(true)
      val xaxis = (0 until signal.size).map(e => e.toDouble).toSeq.toArray
      p.xlabel = "Frequency bin"
      p.ylabel = "Amplitude"
      
      val thresholdPlot = threshold.toSeq
      
      p += plot(xaxis, signal.toArray, name = "FFT input Signal")
      p += plot(xaxis, threshold.toArray, name = "CFAR threshold") //'.'
      p.title_=(s"Constant False Alarm Rate")
      
      f.saveas(s"test_run_dir/CFAR_CA_Scala_Threshold_Plot.pdf")
    }
    (threshold, peakIndices)
  }
  
  // ordered statistic CFAR algorithm
  // can not provide that considerEdges is true
  def cfarOS(signal: Seq[Double], referenceCells: Int, guardCells: Int, scalingFactor: Double, logMode: Boolean = false, plotEn: Boolean = false): (Seq[Double], Seq[Int]) = {
    val totalCells = signal.size
    val windowCells = referenceCells + guardCells
    var threshold : Seq[Double] = Seq()
    var peakIndices: Seq[Int] = Seq()
    
    for (ix <- windowCells until (totalCells - windowCells)) {
      val lead = signal.slice(ix - windowCells, ix - guardCells)
      val lagg = signal.slice(ix + guardCells + 1, ix + windowCells + 1)

      val thrSorted = (lead ++ lagg).sorted //((1.5*referenceCells).toInt)
      val thr = thrSorted((1.5*referenceCells).toInt)
      val scaledThr = if (logMode) thr + scalingFactor else thr * scalingFactor
      if (scaledThr < signal(ix)) peakIndices = peakIndices :+ ix
      threshold = threshold :+ scaledThr
      if (scaledThr < signal(ix)) peakIndices = peakIndices :+ ix
    }
    if (plotEn == true) {
      val f = Figure()
      val p = f.subplot(0)
      p.legend_=(true)
      val xaxis = (0 until signal.size).map(e => e.toDouble).toSeq.toArray
      p.xlabel = "Frequency bin"
      p.ylabel = "Amplitude"

      val thresholdPlot = threshold.toSeq

      p += plot(xaxis, signal.toArray, name = "FFT input Signal")
      p += plot(xaxis, threshold.toArray, name = "CFAR threshold") //'.'
      p.title_=(s"Constant False Alarm Rate OS")

      f.saveas(s"test_run_dir/CFAR_OS_Scala_Threshold_Plot.pdf")
    }
    val edges: Seq[Double] = Seq.fill(windowCells)(0.0)
    threshold = edges ++ threshold ++ edges

    (threshold, peakIndices)
  }
  
  // generalized ordered statistic CFAR algorithms
  def cfarGOS(signal: Seq[Double], referenceCells: Int, guardCells: Int, indexLagg: Int, indexLead: Int, cfarMode: String, considerEdges: Boolean = false, scalingFactor: Double, logMode: Boolean = false, plotEn: Boolean = false) : (Seq[Double], Seq[Int]) = {
    val totalCells = signal.size
    val windowCells = referenceCells + guardCells
    var threshold : Seq[Double] = Seq()
    var peakIndices: Seq[Int] = Seq()
    // This should be checked!
    if (considerEdges) {
      var thr: Double = 0.0
      // take into account only one reference window for the edge cells
      for (ix <- 0 until totalCells) {
        val lead = if (ix < windowCells) None else Some(signal.slice(ix - windowCells, ix - guardCells).sorted(Ordering.Double.reverse))
        val lagg = if (ix >= (totalCells - windowCells)) None else Some(signal.slice(ix + guardCells + 1, ix + windowCells + 1).sorted(Ordering.Double.reverse))

        if (ix < windowCells) {
          thr = lagg.get(indexLagg - 1)
        }
        else if (ix >= (totalCells - windowCells)) {
          thr = lead.get(indexLead - 1)
        }
        else {
          thr = cfarMode match {
                  case "Cell Averaging" => (lead.get(indexLead - 1) + lagg.get(indexLagg - 1))/2
                  case "Smallest Of" => Seq(lead.get(indexLead - 1), lagg.get(indexLagg - 1)).min
                  case "Greatest Of" => Seq(lead.get(indexLead - 1), lagg.get(indexLagg - 1)).max
                  case _ =>  throw new Exception(s"Unknown CFAR type, try with Cell Averaging, Smallest Of or Greatest Of")
                }
        }
        val scaledThr = if (logMode) thr + scalingFactor else thr * scalingFactor

        threshold = threshold :+ scaledThr
        if (scaledThr < signal(ix)) peakIndices :+ ix
      }
    }
    else {
      // this is ok!
      for (ix <- windowCells until (totalCells - windowCells)) {
        val lead = signal.slice(ix - windowCells, ix - guardCells).sorted(Ordering.Double.reverse)
        val lagg = signal.slice(ix + guardCells + 1, ix + windowCells + 1).sorted(Ordering.Double.reverse)
        val thr = cfarMode match {
                    case "Cell Averaging" => (lead(indexLead - 1) + lagg(indexLagg - 1))/2
                    case "Smallest Of" => Seq(lead(indexLead - 1), lagg(indexLagg - 1)).min
                    case "Greatest Of" => Seq(lead(indexLead - 1), lagg(indexLagg - 1)).max
                    case _ =>  throw new Exception(s"Unknown CFAR type, try with Cell Averaging, Smallest Of or Greatest Of")
                  }
        // println("Current threshold is:")
        // println(thr)
        val scaledThr = if (logMode) thr + scalingFactor else thr * scalingFactor
        if (scaledThr < signal(ix)) peakIndices = peakIndices :+ ix
        threshold = threshold :+ scaledThr
        if (scaledThr < signal(ix)) peakIndices = peakIndices :+ ix
      }
    }
    
    if (plotEn == true) {
      val f = Figure()
      val p = f.subplot(0)
      p.legend_=(true)
      val xaxis = (0 until signal.size).map(e => e.toDouble).toSeq.toArray
      p.xlabel = "Frequency bin"
      p.ylabel = "Amplitude"

      val thresholdPlot = threshold.toSeq

      p += plot(xaxis, signal.toArray, name = "FFT input Signal")
      p += plot(xaxis, threshold.toArray, name = "CFAR threshold") //'.'
      p.title_=(s"Constant False Alarm Rate GOS")

      f.saveas(s"test_run_dir/CFAR_GOS_Scala_Threshold_Plot.pdf")
    }
    val edges: Seq[Double] = Seq.fill(windowCells)(0.0)
    threshold = edges ++ threshold ++ edges

    (threshold, peakIndices)
  }
}


// Copied from sdf-fft utils
object RegEnableWithReset {

  /** Returns a register with the specified next, update enable gate, manual reset and reset initialization.
    *
    * @example {{{
    * val regWithEnableAndReset = RegEnable(nextVal, 0.U, reset, ena)
    * }}}
    */
  def apply[T <: Data](next: T, init: T, reset: Bool, enable: Bool): T = {
    val r = RegInit(init)
    
    when (reset) { r := init }
    .elsewhen (enable) { r := next }
    r
  }
}

object ShiftRegisterWithReset
{
  /** Returns the n-cycle delayed version of the input signal.
    *
    * @param in input to delay
    * @param n number of cycles to delay
    * @param resetData reset value for each register in the shift
    * @param reset manual reset
    * @param en enable the shift
    */
  def apply[T <: Data](in: T, n: Int, resetData: T, reset: Bool, en: Bool = true.B): T = {
    // The order of tests reflects the expected use cases.
    if (n != 0) {
      RegEnableWithReset(apply(in, n-1, resetData, reset, en), resetData, reset, en)
    } else {
      in
    }
  }
}

// maybe reset initialization is not necessary
object AdjustableShiftRegister {

  private def applyReccursive[T <: Data](in: T, n: Int, activeRegs: Vec[Bool], out: Vec[T], resetData: T, en: Bool = true.B): T = {
    if (n != 0) {
      val enShift = activeRegs(n-1) && en
      out(n-1) := RegEnable(applyReccursive(in, n - 1, activeRegs, out, resetData, en), resetData, enShift)
      out(n-1)
    }
    else
      in
  }
  
  def apply[T <: Data](in: T, maxDepth: Int, depth: UInt, resetData: T, en: Bool = true.B): T = {
    require(maxDepth > 0)
    requireIsHardware(in)
    assert(depth <= maxDepth.U)
    
    val activeRegs = Wire(Vec(maxDepth, Bool()))
//     activeRegs.suggestName("activeRegs")
//     dontTouch(activeRegs)
    activeRegs.zipWithIndex.map { case (active, index) => active := (index.U <= depth - 1.U).asBool }
    val out = Wire(Vec(maxDepth, in.cloneType))
    applyReccursive(in, maxDepth, activeRegs, out, resetData, en)
    out(depth - 1.U)
  }
  
  //not sure is this right way to do this
  def returnOut[T <: Data](in: T, maxDepth: Int, depth: UInt, resetData: T, en: Bool = true.B): (T, Vec[T]) = {
    require(maxDepth > 0)
    requireIsHardware(in)
    assert(depth <= maxDepth.U)
    
    val activeRegs = Wire(Vec(maxDepth, Bool()))
    dontTouch(activeRegs)
    activeRegs.suggestName("activeRegs")
    activeRegs.zipWithIndex.map { case (active, index) => active := (index.U <= depth - 1.U).asBool }
    //activeRegs.zipWithIndex.map { case (active, index) => active := Mux(depth === 0.U, false.B, (index.U <= depth - 1.U).asBool) }

    val out = Wire(Vec(maxDepth, in.cloneType))
    applyReccursive(in, maxDepth, activeRegs, out, resetData, en)
    (out(depth - 1.U), out)
  }
}

// this module should instatiate AdjustableShiftRegister and wrap it with AXI Stream interface
// it is assumed that signal lastIn triggers flushing
class AdjustableShiftRegisterStream[T <: Data](val proto: T, val maxDepth: Int, val parallelOut: Boolean = false, val sendCnt: Boolean = false, val enInitStore: Boolean = true) extends Module { //Stream
  require(maxDepth > 1, s"Depth must be > 1, got $maxDepth")
  requireIsChiselType(proto)

  val io = IO(new Bundle {
    val depth       = Input(UInt(log2Ceil(maxDepth + 1).W))
    val in          = Flipped(Decoupled(proto.cloneType))
    val lastIn      = Input(Bool())

    val out         = Decoupled(proto.cloneType)
    val lastOut     = Output(Bool())
    val parallelOut = Output(Vec(maxDepth, proto))
    val cnt         = if (sendCnt) Some(Output(UInt(log2Ceil(maxDepth + 1).W))) else None

    val regFull     = Output(Bool())
    val regEmpty    = Output(Bool())
  })
  
  val initialInDone = RegInit(false.B)
  val last = RegInit(false.B)

  dontTouch(initialInDone)
  initialInDone.suggestName("InitialInDone")
  val resetData = 0.U.asTypeOf(io.in.bits)
  val en = io.in.fire() || (last && io.out.ready)
  // or drop data if io.out.ready is inactive, in that case en is:
  //val en = io.in.fire() || last
  
  val (adjShiftReg, adjShiftRegOut) = AdjustableShiftRegister.returnOut(io.in.bits, maxDepth, io.depth, resetData, en)
  val cntIn  = RegInit(0.U(log2Ceil(maxDepth + 1).W))
  
  when (io.lastIn && io.in.fire()) {
    last := true.B
  }
  
  when (io.in.fire()) {
    cntIn := cntIn + 1.U
  }
  // if depth is one
  when (io.depth > 1.U) {
    when (cntIn === io.depth - 1.U && io.in.fire()) {
      initialInDone := true.B
    }
  }
  .otherwise {
    when (io.in.fire() && io.depth === 1.U) {
      initialInDone := true.B
    }
  }
  
  val fireLastIn = io.lastIn && io.in.fire()
  val lastOut = AdjustableShiftRegister(fireLastIn, maxDepth, io.depth, resetData = false.B, en = io.out.fire())
  
  when (lastOut && io.out.fire()) {
    initialInDone := false.B
    last := false.B
    cntIn := 0.U
  }
  
  io.regEmpty  := cntIn === 0.U && ~initialInDone
  io.regFull   := initialInDone && ~last
  //io.in.ready    := ~initialInDone || io.out.ready && ~last // or without ~last
  
  ///////////////// CHANGED /////////////////////////
  if (enInitStore) {
    io.in.ready    :=  Mux(io.depth === 0.U, io.out.ready, ~initialInDone || io.out.ready && ~last)
  }
  else {
    io.in.ready    :=  Mux(io.depth === 0.U, io.out.ready, io.out.ready && ~last)
  }
  
  dontTouch(io.parallelOut)
  io.out.bits    := Mux(io.depth === 0.U, io.in.bits, adjShiftReg)
  io.parallelOut := adjShiftRegOut // parallel output is not important
  io.lastOut     := Mux(io.depth === 0.U, io.lastIn && io.in.fire(), lastOut)
  io.out.valid   := Mux(io.depth === 0.U, io.in.valid, initialInDone && io.in.valid || (last && en))
  if (sendCnt)
    io.cnt.get := cntIn
}

// simple RegEnable wrapped with AXI Stream interface
// it is assumed that signal lastIn triggers flushing
class CellUnderTest[T <: Data](val proto: T) extends Module {
  requireIsChiselType(proto)

  val io = IO(new Bundle {
    val in      = Flipped(Decoupled(proto.cloneType))
    val lastIn  = Input(Bool())
    val out     = Decoupled(proto.cloneType)
    val lastOut = Output(Bool())
  })
  
  val initialInDone = RegInit(false.B)
  val resetData = 0.U.asTypeOf(io.in.bits)
  val last = RegInit(false.B)
  
  when (io.in.fire() && ~initialInDone) {
    initialInDone := true.B
  }
  
  val en = io.in.fire() || (last && io.out.ready)
  // or drop data if io.out.ready is inactive in that case 
  //val en = io.in.fire() || last 
  
  val cut = RegEnable(io.in.bits, resetData, en)
  val cntIn  = RegInit(0.U(1.W))
  
  when (io.lastIn && io.in.fire()) {
    last := true.B 
  }
  
  val fireLastIn = io.lastIn && io.in.fire() 
  val lastOut = RegEnable(fireLastIn, false.B, io.out.ready)
  
  when (lastOut && io.out.ready) {
    last := false.B
    initialInDone := false.B
  }
  
  io.in.ready    := ~initialInDone || io.out.ready && ~last // or without ~last
  io.out.bits    := cut
  io.lastOut     := lastOut
  io.out.valid   := initialInDone && io.in.valid || (last && en)
}

object AdjShiftApp extends App
{
  chisel3.Driver.execute(args,()=>new AdjustableShiftRegisterStream(UInt(16.W), 6))
}

class ShiftRegisterMemStream[T <: Data](val proto: T, val maxDepth: Int, val enInitStore: Boolean = true) extends Module {
  require(maxDepth > 1, s"Depth must be > 1, got $maxDepth")
  requireIsChiselType(proto)

  val io = IO(new Bundle {
    val depth    = Input(UInt(log2Ceil(maxDepth + 1).W))
    val in       = Flipped(Decoupled(proto.cloneType))
    val lastIn   = Input(Bool())
    val out      = Decoupled(proto.cloneType)
    val lastOut  = Output(Bool())
    val memFull  = Output(Bool())
    val memEmpty = Output(Bool())
  })
  
  val mem           = Mem(maxDepth, proto)
  val readIdx       = Wire(UInt(log2Ceil(maxDepth).W))
  //val readIdxReg    = RegInit(0.U(log2Ceil(maxDepth).W) - (maxDepth - 1).U)
  val writeIdxReg   = RegInit(0.U(log2Ceil(maxDepth).W))
  val last          = RegInit(false.B)
  val initialInDone = RegInit(false.B)
  val en            = io.in.fire() || (last && io.out.ready)
  

  val validPrev = RegNext(io.in.valid, init=false.B)
  when (writeIdxReg + 1.U >= io.depth) {
    readIdx := writeIdxReg - io.depth + 1.U
  }.otherwise {
    readIdx := maxDepth.U + writeIdxReg + 1.U - io.depth
  }
  dontTouch(readIdx)

//   when (en) { //validPrev) {
//     readIdx := Mux(readIdx < (maxDepth - 1).U, readIdx + 1.U, 0.U)
//   } .otherwise {
//     readIdxReg := readIdx
//   }
  
  when (writeIdxReg === io.depth - 1.U && io.in.fire()) {
    initialInDone := true.B
  }
  val fireLastIn = io.lastIn && io.in.fire()
  
  when (fireLastIn) {
    last := true.B
  }
  val lastOut = AdjustableShiftRegister(fireLastIn, maxDepth, io.depth, resetData = false.B, en = io.out.fire())
  val resetAll = lastOut && io.out.fire()
  when (resetAll) {
    initialInDone := false.B
    last := false.B
    writeIdxReg := 0.U
  }
  
  when (en && ~resetAll) {
    mem.write(writeIdxReg, io.in.bits)
    writeIdxReg := Mux(writeIdxReg < (maxDepth - 1).U, writeIdxReg + 1.U, 0.U)
  }
  
  val outputQueue = Module(new Queue(proto, 1, pipe=true, flow=true))
  outputQueue.io.enq.valid := validPrev
  outputQueue.io.enq.bits := RegNext(mem.read(readIdx), 0.U.asTypeOf(proto))
  outputQueue.io.deq.ready := io.in.fire() || (last && io.out.ready) // Check  this!
  
  io.memEmpty  := writeIdxReg === 0.U && ~initialInDone
  io.memFull   := initialInDone && ~last
  //io.in.ready  := ~initialInDone || io.out.ready && ~last
  ////////////////// CHANGED ////////////////////////////
  if (enInitStore) {
    io.in.ready  := ~initialInDone || io.out.ready && ~last //io.out.ready && ~last
  }
  else {
    io.in.ready  := io.out.ready && ~last
  }
  
  io.out.bits  := outputQueue.io.deq.bits //mem.read(readIdx)
  
  io.lastOut   := lastOut
  io.out.valid := initialInDone && io.in.valid || (last && en)
}

object ShiftRegMemApp extends App
{
  chisel3.Driver.execute(args,()=>new ShiftRegisterMemStream(UInt(16.W), 6))
}
