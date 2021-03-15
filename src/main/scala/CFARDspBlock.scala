package cfar

import chisel3._
import chisel3.util._
import chisel3.experimental._
import dsptools._
import dsptools.numbers._

import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._

abstract class CFARBlock [T <: Data : Real: BinaryRepresentation, D, U, E, O, B <: Data] (params: CFARParams[T], beatBytes: Int) extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] with HasCSR {

  val streamNode = AXI4StreamIdentityNode()

  lazy val module = new LazyModuleImp(this) {
    val (in, _)  = streamNode.in(0)
    val (out, _) = streamNode.out(0)

    val cfar = Module(new CFARCore(params))

    // control registers
    val thresholdScalerWidth = params.protoScaler.getWidth
    val divSumWidth = log2Ceil(log2Ceil(params.leadLaggWindowSize + 1))
    val fftWin = RegInit(params.fftSize.U(log2Ceil(params.fftSize + 1).W))
    val thresholdScaler = RegInit(0.U(thresholdScalerWidth.W))
    val logOrLinearMode = RegInit(false.B)
    val divSum = RegInit(0.U(divSumWidth.W))
    
    val peakGrouping = RegInit(false.B)
    val cfarAlgorithm = RegInit(0.U(1.W))
    val cfarMode = RegInit(0.U(2.W))
    val windowCells = RegInit(params.leadLaggWindowSize.U(log2Ceil(params.leadLaggWindowSize + 1).W))
    val guardCells = RegInit(params.guardWindowSize.U(log2Ceil(params.guardWindowSize + 1).W))
    // default take middle value
    val indexLead = RegInit((params.leadLaggWindowSize/2).U(log2Ceil(params.leadLaggWindowSize + 1).W))
    val indexLagg = RegInit((params.leadLaggWindowSize/2).U(log2Ceil(params.leadLaggWindowSize + 1).W))
    // sub window size - used only for CASH algorithm
    val subWindowSize = RegInit(params.leadLaggWindowSize.U(log2Ceil(params.leadLaggWindowSize + 1).W))
    //val detectedPeaksList = RegInit(0.U(params.fftSize.W)) // it will use a lot of registers
    val detectedPeaksListTmp = RegInit(VecInit(Seq.fill(params.fftSize)(false.B)))

    // connect input
    cfar.io.in.valid := in.valid
    cfar.io.in.bits := in.bits.data.asTypeOf(params.protoIn)
    in.ready := cfar.io.in.ready
    cfar.io.fftWin := fftWin
    cfar.io.thresholdScaler := thresholdScaler.asTypeOf(params.protoScaler)
    cfar.io.logOrLinearMode := logOrLinearMode
    
    if (params.CFARAlgorithm != GOSCFARType) {
      cfar.io.divSum.get := divSum
    }
    if (params.includeCASH == true) {
      cfar.io.subCells.get := subWindowSize
    }

    cfar.io.peakGrouping := peakGrouping
    if (params.CFARAlgorithm == GOSCACFARType) {
      cfar.io.cfarAlgorithm.get := cfarAlgorithm
    }
    cfar.io.cfarMode    := cfarMode
    cfar.io.windowCells := windowCells
    cfar.io.guardCells  := guardCells
    cfar.io.lastIn      := in.bits.last
    
    if (params.CFARAlgorithm != CACFARType) {
      cfar.io.indexLagg.get := indexLagg
      cfar.io.indexLead.get := indexLead
    }
  
    val detectedPeaksList = detectedPeaksListTmp.asUInt
    
    if (params.sendCut) {
      out.bits.data := cfar.io.out.bits.cut.get.asUInt
      // This should be organized differently
      // It is not possible to save peak list in registers
      when (cfar.io.fftBin === fftWin - 1.U) {
        detectedPeaksListTmp.map(c => false.B)
      }
      .elsewhen (cfar.io.out.bits.peak) {
        detectedPeaksListTmp(cfar.io.fftBin) := true.B
      }
    }
    else {
      out.bits.data    := Cat(cfar.io.out.bits.threshold.asUInt, cfar.io.fftBin, cfar.io.out.bits.peak) // output.asUInt
    }
    out.valid          := cfar.io.out.valid
    cfar.io.out.ready  := out.ready
    out.bits.last := cfar.io.lastOut

    val fields = Seq(
      RegField(log2Ceil(params.fftSize + 1), fftWin,
        RegFieldDesc(name = "fftWin", desc = "Determines fft size")),
      RegField(thresholdScalerWidth, thresholdScaler,
        RegFieldDesc(name = "thresholdScaler", desc = "Threshold scaler")),
      RegField(1, logOrLinearMode,
        RegFieldDesc(name = "logOrLinearMode", desc = "Denotes whether input data is in log or in linear mode")),
      RegField(divSumWidth, divSum,
        RegFieldDesc(name = "divSum", desc = "Denotes div factor of the sliding sum when CACFAR algorithm is used")),
      RegField(1, peakGrouping,
        RegFieldDesc(name = "peakGrouping", desc = "Use peak grouping logic for peak detection")),
      RegField(1, cfarAlgorithm,
        RegFieldDesc(name = "cfarAlgorithm", desc = "Defines cfar algorithm, used only if parameter CFARAlgorithm is set to GOSCACFARType")),
      RegField(2, cfarMode,
        RegFieldDesc(name = "cfarMode", desc = "Defines cfar mode CA (cell averaging), GO (greatest of) or SO (smallest of)")),
      RegField(log2Ceil(params.leadLaggWindowSize + 1), windowCells,
        RegFieldDesc(name = "windowCells", desc = "Defines size of the leading and lagging window")),
      RegField(log2Ceil(params.guardWindowSize + 1), guardCells,
        RegFieldDesc(name = "guardCells", desc = "Defines size of the guard window")),
      RegField(log2Ceil(params.leadLaggWindowSize + 1), indexLagg,
        RegFieldDesc(name = "indexLagg", desc = "Defines index of the sorted cells inside lagging window used for the threshold calculation")),
      RegField(log2Ceil(params.leadLaggWindowSize + 1), indexLead,
        RegFieldDesc(name = "indexLead", desc = "Defines index of the sorted cells inside leading window used for the threshold calculation")),
      RegField(log2Ceil(params.leadLaggWindowSize + 1) , subWindowSize,
        RegFieldDesc(name = "subWindowSize", desc = "Defines sub-window size used in CASH algorithm"))
      
//      RegField.r(params.fftSize/32, detectedPeaksListTmp.asUInt,
//         RegFieldDesc(name = "detectedPeaksList", desc = "Detected peaks list"))
     )
    //define abract register map so it can be AXI4, Tilelink, APB, AHB
    regmap(
      fields.zipWithIndex.map({ case (f, i) =>
          i * beatBytes -> Seq(f)
      }): _*
    )
  }
}

class AXI4CFARBlock[T <: Data : Real: BinaryRepresentation](params: CFARParams[T], address: AddressSet, _beatBytes: Int = 4)(implicit p: Parameters) extends CFARBlock[T, AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params, _beatBytes) with AXI4DspBlock with AXI4HasCSR {
  val mem = Some(AXI4RegisterNode(address = address, beatBytes = _beatBytes)) // use AXI4 memory mapped
}

object CFARDspBlock extends App
{
  val paramsCFAR = CFARParams(
    protoIn = FixedPoint(16.W, 0.BP),
    protoThreshold = FixedPoint(16.W, 0.BP),
    protoScaler = FixedPoint(16.W, 0.BP),
    leadLaggWindowSize = 64,
    guardWindowSize = 4,
    fftSize = 1024,
    sendCut = false,
    minSubWindowSize = None,
    includeCASH = false,
    CFARAlgorithm = CACFARType
  )

  val baseAddress = 0x500
  implicit val p: Parameters = Parameters.empty
  val cfarModule = LazyModule(new AXI4CFARBlock(paramsCFAR, AddressSet(baseAddress + 0x100, 0xFF), _beatBytes = 4) with dspblocks.AXI4StandaloneBlock {
    override def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
  })

  chisel3.Driver.execute(args, ()=> cfarModule.module)
}
