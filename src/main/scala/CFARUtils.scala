package cfar

import chisel3._
import chisel3.util._
import chisel3.internal.{requireIsChiselType, requireIsHardware}

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
  // returns parallel output of the ShiftRegister also
//   def apply[T <: Data](in: T, maxDepth: Int, depth: UInt, resetData: T, parallelOut: Boolean, en: Bool = true.B): (T, Vec[T]) = {
//     require(maxDepth > 0)
//     requireIsHardware(in)
//     assert(depth <= maxDepth.U)
//     
//     val activeRegs = Wire(Vec(maxDepth, Bool()))
//     activeRegs.zipWithIndex.map { case (active, index) => active := (index.U <= depth - 1.U).asBool }
//     val out = Wire(Vec(maxDepth, in.cloneType))
//     applyReccursive(in, maxDepth, activeRegs, out, resetData, en)
//     (out(depth - 1.U), out)
//   }
  
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
    activeRegs.zipWithIndex.map { case (active, index) => active := (index.U <= depth - 1.U).asBool }
    val out = Wire(Vec(maxDepth, in.cloneType))
    applyReccursive(in, maxDepth, activeRegs, out, resetData, en)
    (out(depth - 1.U), out)
  }
}

// this module should instatiate AdjustableShiftRegister and wrap it with AXI Stream interface
// it is assumed that signal lastIn triggers flushing
class AdjustableShiftRegisterStream[T <: Data](val proto: T, val maxDepth: Int, val parallelOut: Boolean = false) extends Module { //Stream
  require(maxDepth > 1, s"Depth must be > 1, got $maxDepth")
  requireIsChiselType(proto)

  val io = IO(new Bundle {
    val depth       = Input(UInt(log2Ceil(maxDepth + 1).W))
    val in          = Flipped(Decoupled(proto.cloneType))
    val lastIn      = Input(Bool())
    
    val out         = Decoupled(proto.cloneType)
    val lastOut     = Output(Bool())
    val parallelOut = Output(Vec(maxDepth, proto))
  })
  
  val initialInDone = RegInit(false.B)
  val last = RegInit(false.B)

  dontTouch(initialInDone)
  initialInDone.suggestName("InitialInDone")
  val resetData = 0.U.asTypeOf(io.in.bits)
  val en = io.in.fire() || (last && io.out.ready)
  // or drop data if io.out.ready is inactive in that case en is:
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
 
  io.in.ready    := ~initialInDone || io.out.ready && ~last // or without ~last
  io.out.bits    := adjShiftReg
  io.parallelOut := adjShiftRegOut
  io.lastOut     := lastOut
  io.out.valid   := initialInDone && io.in.valid || (last && en)
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

class ShiftRegisterMemStream[T <: Data](val proto: T, val maxDepth: Int) extends Module {
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
  
  val mem           = SyncReadMem(maxDepth, proto)
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
  
  when (lastOut && io.out.fire()) {
    initialInDone := false.B
    last := false.B
    writeIdxReg := 0.U
  }
  
  when (en) {
    mem.write(writeIdxReg, io.in.bits)
    writeIdxReg := Mux(writeIdxReg < (maxDepth - 1).U, writeIdxReg + 1.U, 0.U)
  }
  
  val outputQueue = Module(new Queue(proto, 1, pipe=true, flow=true))
  outputQueue.io.enq.valid := validPrev
  outputQueue.io.enq.bits := mem.read(readIdx)
  outputQueue.io.deq.ready := io.in.fire() || (last && io.out.ready) // Check  this!
  
  
  io.memEmpty  := writeIdxReg === 0.U && ~initialInDone
  io.memFull   := initialInDone && ~last
  io.in.ready  := ~initialInDone || io.out.ready && ~last
  io.out.bits  := outputQueue.io.deq.bits //mem.read(readIdx)
  
  io.lastOut   := lastOut
  io.out.valid := initialInDone && io.in.valid || (last && en)
}

object ShiftRegMemApp extends App
{
  chisel3.Driver.execute(args,()=>new ShiftRegisterMemStream(UInt(16.W), 6))
}
