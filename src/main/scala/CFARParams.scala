package cfar

import chisel3._
import chisel3.experimental._

import dsptools._
import dsptools.numbers._

sealed trait CFARType

// Linear insertion sorters are used for storage cells
// Generalized order statistic CFAR algorithms
case object GOSCFARType extends CFARType

// Cell avaraging  CFAR algorithms
// cfar.ShiftRegisterMemStream used for storage cells
case object CACFARType  extends CFARType 

// Linear insertion sorters are used for storage cells
// Design supports both GOS and CA CFAR algorithm
case object GOSCACFARType extends CFARType 

case class CFARParams[T <: Data: Real](
  protoIn               : T, // Data type of the input data
  protoThreshold        : T, // Data type of the threshold
  protoScaler           : T, // Data type of the scaler
  CFARAlgorithm         : CFARType = GOSCACFARType, // CFAR algorithm
  leadLaggWindowSize    : Int = 16,   // maximum number of leading/lagging cells
  guardWindowSize       : Int = 4,    // maximum number of guard cells
  fftSize               : Int = 1024, // maximum fft size
  numAddPipes           : Int = 0,    // number of add pipeline registers
  numMulPipes           : Int = 0     // number of mull pipeline registers
  ) {
  // requireIsPow2?
  require(leadLaggWindowSize > 0 & guardWindowSize > 0)
  require(leadLaggWindowSize  > guardWindowSize)
  
  requireIsChiselType(protoIn)
  requireIsChiselType(protoThreshold)
  requireIsChiselType(protoScaler)
 }
 
