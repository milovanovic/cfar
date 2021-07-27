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


trait AXI4CFARStandaloneBlock extends AXI4CFARBlock[FixedPoint] {
  def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
  val ioMem = mem.map { m => {
    val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))

    m :=
      BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) :=
      ioMemNode

    val ioMem = InModuleBody { ioMemNode.makeIO() }
    ioMem
  }}
  
  val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 2)))
  val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()

  ioOutNode :=
    AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) :=
    streamNode :=
    BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 2)) :=
    ioInNode

  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }
}

abstract class CFARBlock [T <: Data : Real: BinaryRepresentation, D, U, E, O, B <: Data] (params: CFARParams[T], beatBytes: Int) extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] with HasCSR {

  val outputWidthMin = if (params.sendCut)
                          params.protoThreshold.getWidth + params.protoIn.getWidth + log2Ceil(params.fftSize) + 1
                       else
                          params.protoThreshold.getWidth + log2Ceil(params.fftSize) + 1

  val masterParams = AXI4StreamMasterParameters(
    name = "AXI4 Stream xWrDataPreProc",
    n = (outputWidthMin + 8 - 1) / 8,
    numMasters = 1
  )
  println(((outputWidthMin + 8 -1)/8).toString)
  val slaveParams = AXI4StreamSlaveParameters()
  val slaveNode  = AXI4StreamSlaveNode(slaveParams)
  val masterNode = AXI4StreamMasterNode(masterParams)
  
  val streamNode = NodeHandle(slaveNode, masterNode)

  lazy val module = new LazyModuleImp(this) {
    val (in, edgeIn) = slaveNode.in.head
    val (out, edgeOut) = masterNode.out.head

    val cfar = Module(new CFARCore(params))

    // control registers
    val thresholdScalerWidth = params.protoScaler.getWidth
    val divSumWidth = log2Ceil(log2Ceil(params.leadLaggWindowSize + 1))
    
    val fftWin = RegInit(params.fftSize.U(log2Ceil(params.fftSize + 1).W))
    val thresholdScaler = RegInit(0.U(thresholdScalerWidth.W))
    val peakGrouping = RegInit(false.B)
    val cfarMode = RegInit(0.U(2.W))
    val windowCells = RegInit(params.leadLaggWindowSize.U(log2Ceil(params.leadLaggWindowSize + 1).W))
    val guardCells = RegInit(params.guardWindowSize.U(log2Ceil(params.guardWindowSize + 1).W))

    val detectedPeaksListTmp = RegInit(VecInit(Seq.fill(params.fftSize)(false.B)))
    
    // common fields for all CFAR types
    var commonFields = Seq(
      RegField(log2Ceil(params.fftSize + 1), fftWin,
        RegFieldDesc(name = "fftWin", desc = "Determines fft size")),
      RegField(thresholdScalerWidth, thresholdScaler,
        RegFieldDesc(name = "thresholdScaler", desc = "Threshold scaler")),
      RegField(1, peakGrouping,
        RegFieldDesc(name = "peakGrouping", desc = "Use peak grouping logic for peak detection")),
      RegField(2, cfarMode,
        RegFieldDesc(name = "cfarMode", desc = "Defines cfar mode CA (cell averaging), GO (greatest of) or SO (smallest of) or CASH (when cash is included)")),
      RegField(log2Ceil(params.leadLaggWindowSize + 1), windowCells,
        RegFieldDesc(name = "windowCells", desc = "Defines size of the leading and lagging window")),
      RegField(log2Ceil(params.guardWindowSize + 1), guardCells,
        RegFieldDesc(name = "guardCells", desc = "Defines size of the guard window")))

    // connect input
    cfar.io.in.valid := in.valid
    cfar.io.in.bits := in.bits.data.asTypeOf(params.protoIn)
    in.ready := cfar.io.in.ready
    cfar.io.fftWin := fftWin
    cfar.io.thresholdScaler := thresholdScaler.asTypeOf(params.protoScaler)
    
    if (params.logOrLinReg) {
      val logOrLinearMode = RegInit(false.B)
      logOrLinearMode.suggestName("logOrLinearMode")
      cfar.io.logOrLinearMode.get := logOrLinearMode
      commonFields = commonFields :+ RegField(1, logOrLinearMode,
        RegFieldDesc(name = "logOrLinearMode", desc = "Denotes whether input data is in log or in linear mode"))
    }
    if (params.CFARAlgorithm != GOSCFARType) {
      val divSum = RegInit(0.U(divSumWidth.W))
      cfar.io.divSum.get := divSum
      commonFields = commonFields :+ RegField(divSumWidth, divSum,
        RegFieldDesc(name = "divSum", desc = "Denotes div factor of the sliding sum when CACFAR algorithm is used"))
    }
    if (params.includeCASH == true) {
      //val subWindowSize = RegInit(params.leadLaggWindowSize.U(log2Ceil(params.leadLaggWindowSize + 1).W)) //RegInit(params.leadLaggWindowSize.U(log2Ceil(params.leadLaggWindowSize + 1).W))
      val subWindowSize = RegInit(0.U(log2Ceil(params.leadLaggWindowSize + 1).W))
      subWindowSize.suggestName("subWindowSize")
      commonFields = commonFields :+ RegField(log2Ceil(params.leadLaggWindowSize + 1) , subWindowSize,
        RegFieldDesc(name = "subWindowSize", desc = "Defines sub-window size used in CASH algorithm"))
      cfar.io.subCells.get := subWindowSize
    }
    cfar.io.peakGrouping := peakGrouping

    if (params.CFARAlgorithm == GOSCACFARType) {
      val cfarAlgorithm = RegInit(0.U(1.W))
      cfar.io.cfarAlgorithm.get := cfarAlgorithm
      commonFields = commonFields :+ RegField(1, cfarAlgorithm,
        RegFieldDesc(name = "cfarAlgorithm", desc = "Defines cfar algorithm, used only if parameter CFARAlgorithm is set to GOSCACFARType"))
    }
    cfar.io.cfarMode    := cfarMode
    cfar.io.windowCells := windowCells
    cfar.io.guardCells  := guardCells
    cfar.io.lastIn      := in.bits.last
    
    if (params.CFARAlgorithm != CACFARType) {
      val indexLead = RegInit((params.leadLaggWindowSize/2).U(log2Ceil(params.leadLaggWindowSize + 1).W))
      val indexLagg = RegInit((params.leadLaggWindowSize/2).U(log2Ceil(params.leadLaggWindowSize + 1).W))
      cfar.io.indexLagg.get := indexLagg
      cfar.io.indexLead.get := indexLead
      commonFields = commonFields :+ RegField(log2Ceil(params.leadLaggWindowSize + 1), indexLagg,
        RegFieldDesc(name = "indexLagg", desc = "Defines index of the sorted cells inside lagging window used for the threshold calculation"))
      commonFields = commonFields :+ RegField(log2Ceil(params.leadLaggWindowSize + 1), indexLead,
        RegFieldDesc(name = "indexLead", desc = "Defines index of the sorted cells inside leading window used for the threshold calculation"))
    }
    if (params.edgesMode == AllEdgeModes) {
      val edgesMode = RegInit(0.U(2.W)) // in the future maybe extended to 3.W!
      commonFields = commonFields :+ RegField(2, edgesMode,
        RegFieldDesc(name = "edgesMode", desc = "Defines calculating threshold mode"))
      cfar.io.edgesMode.get :=  edgesMode
    }

    if (params.sendCut) {
//      require(cfar.io.out.bits.threshold.getWidth + cfar.io.out.bits.cut.get.getWidth + cfar.io.fftBin.getWidth + 1 < 48)
      out.bits.data := Cat(cfar.io.out.bits.threshold.asUInt, cfar.io.out.bits.cut.get.asUInt, cfar.io.fftBin, cfar.io.out.bits.peak)
    }
    else {
      out.bits.data    := Cat(cfar.io.out.bits.threshold.asUInt, cfar.io.fftBin, cfar.io.out.bits.peak) // output.asUInt
    }
    out.valid          := cfar.io.out.valid
    cfar.io.out.ready  := out.ready
    out.bits.last := cfar.io.lastOut

    //define abract register map so it can be AXI4, Tilelink, APB, AHB
    regmap(
      commonFields.zipWithIndex.map({ case (f, i) =>
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
    logOrLinReg = true,
    fftSize = 1024,
    sendCut = false,
    minSubWindowSize = Some(8),
    includeCASH = true,
    CFARAlgorithm = CACFARType
  )

  val baseAddress = 0x500
  implicit val p: Parameters = Parameters.empty
  val cfarModule = LazyModule(new AXI4CFARBlock(paramsCFAR, AddressSet(baseAddress + 0x100, 0xFF), _beatBytes = 4) with dspblocks.AXI4StandaloneBlock {
    override def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
  })

  chisel3.Driver.execute(args, ()=> cfarModule.module)
}
