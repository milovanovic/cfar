package cfar

import chisel3._
import chisel3.experimental._
import chisel3.util._

import dsptools._
import dsptools.numbers._

import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.internal.requireIsHardware

class CFAROutFields [T <: Data: Real] (protoIn: T, protoThreshold: T) extends Bundle {
  requireIsChiselType(protoIn)
  requireIsChiselType(protoThreshold)
  
  val peak           = Output(Bool())         // define whether the cut is peak or not
  val cut            = Output(protoIn)        // cell under test
  val threshold      = Output(protoThreshold) // threshold
  
  override def cloneType: this.type = new CFAROutFields(protoIn, protoThreshold).asInstanceOf[this.type] // must have cloneType
}

// object CFAROutFields {
//   def apply[T <: Data: Real] (protoIn: T, protoThreshold: T) =
//     new CFAROutFields(protoIn, protoThreshold)
// }

class CFARIO [T <: Data: Real](params: CFARParams[T]) extends Bundle {
  val in = Flipped(Decoupled(params.protoIn))
  val lastIn = Input(Bool())
  
  // control registers
  val fftWin = Input(UInt(log2Ceil(params.fftSize + 1).W))
  val thresholdScaler = Input(params.protoScaler)
  val logOrLinearMode = Input(Bool())
  val divSum = if (params.CFARAlgorithm != GOSCFARType) Some(Input(UInt(log2Ceil(log2Ceil(params.leadLaggWindowSize + 1)).W))) else None
  
  val peakGrouping = Input(Bool())
  val cfarAlgorithm = if (params.CFARAlgorithm == GOSCACFARType) Some(Input(UInt(1.W))) else None // change thid to Bool!
  val cfarMode = Input(UInt(2.W))
  val windowCells = Input(UInt(log2Ceil(params.leadLaggWindowSize + 1).W))
  val guardCells = Input(UInt(log2Ceil(params.guardWindowSize + 1).W))
  
  // Ordered statistic CFAR specific control registers
  val indexLead = if (params.CFARAlgorithm != CACFARType) Some(Input(UInt(log2Ceil(params.leadLaggWindowSize + 1).W))) else None // Check here log2Ceil
  val indexLagg = if (params.CFARAlgorithm != CACFARType)  Some(Input(UInt(log2Ceil(params.leadLaggWindowSize + 1).W))) else None

  val out = Decoupled(new CFAROutFields(params.protoIn, params.protoThreshold))
  val lastOut = Output(Bool())
  val fftBin  = Output(UInt(log2Ceil(params.fftSize).W))
 
  override def cloneType: this.type = CFARIO(params).asInstanceOf[this.type]
}

object CFARIO {
  def apply[T <: Data: Real](params: CFARParams[T]): CFARIO[T] = new CFARIO(params)
}

class CFARCore[T <: Data : Real : BinaryRepresentation](val params: CFARParams[T]) extends Module {
  val io = IO(CFARIO(params))
  val cfarCore = if (params.CFARAlgorithm == CACFARType) Module(new CFARCoreWithMem(params)) else Module(new CFARCoreWithLis(params))
  

  // just instatiate appropriate design
  
  cfarCore.io.in <> io.in 
  cfarCore.io.lastIn := io.lastIn
  cfarCore.io.fftWin := io.fftWin
  cfarCore.io.thresholdScaler := io.thresholdScaler
  cfarCore.io.logOrLinearMode := io.logOrLinearMode
  
  if (params.CFARAlgorithm != GOSCFARType) {
    cfarCore.io.divSum.get := io.divSum.get
  }
  cfarCore.io.peakGrouping := io.peakGrouping
  if (params.CFARAlgorithm == GOSCACFARType) {
    cfarCore.io.cfarAlgorithm.get := io.cfarAlgorithm.get
  }
  cfarCore.io.cfarMode := io.cfarMode
  cfarCore.io.windowCells := io.windowCells
  cfarCore.io.guardCells := io.guardCells
  
  if (params.CFARAlgorithm != CACFARType) {
    cfarCore.io.indexLagg.get := io.indexLagg.get
    cfarCore.io.indexLead.get := io.indexLead.get
  }
  
  cfarCore.io.out <> io.out
  io.lastOut := cfarCore.io.lastOut
  io.fftBin := cfarCore.io.fftBin
}


object CFARCoreApp extends App
{
  val params: CFARParams[FixedPoint] =  CFARParams(
    protoIn = FixedPoint(16.W, 8.BP),
    protoThreshold = FixedPoint(16.W, 8.BP), // output thres
    protoScaler = FixedPoint(16.W, 8.BP),
    CFARAlgorithm = CACFARType
   // other parameters are default
  )
  
  chisel3.Driver.execute(args,()=>new CFARCore(params))
}



