

Constant false alarm rate (CFAR) Chisel generator
======================================================

[![Build Status](https://travis-ci.org/milovanovic/cfar.svg?branch=master)](https://travis-ci.org/milovanovic/cfar)

## Overview

This repository contains a generator of parameterizable and runtime reconfigurable Constant False Alarm Rate (CFAR) engines writen in [Chisel](http://www.chisel-lang.org) hardware design language. CFAR processors are used in radar digital signal processing systems as an indispensable block for object detection in cluttered and noisy environments. Proposed Chisel generator implements six different variants of the CFAR algorithms based on linear and nonlinear operations.

### CFAR Chisel generator

CFAR generator and its interface showing inout signals as well as control and status registers is shown in the figure below. ![Interface of the CFAR Generator](./doc/images/CFARBlock.svg)
Presented hardware architecture supports six variants  of the CFAR algorithms which can be switched during run time configurability. All those algorithms are implemented in the way that input power samples (`in_data`) are shifted through the lagging/leading windows, guard cells and the cell under test (CUT), adaptive threshold is continuously calculated and CUT value is compared against obtained threshold to decide whether a peak is detected or not in present cell.

Supported CFAR algorithms are listed below:
Cell Averaging (CA) CFAR algorithms:
* Classical CA-CFAR
* Greatest of (GO) CA-CFAR
* Smallest of  (SO) CA-CFAR

Generalised Ordered Statistic (GOS) CFAR algorithms:

* Cell Averaging (CA) GOS-CFAR
* Greatest of (GO) GOS-CFAR
* Smallest of (SO) COS-CFAR

Since Generalised Ordered Statistic CFAR algorithms require sorting  input samples, [A Linear Insertion Sorter (LIS) Chisel Generator](https://github.com/milovanovic/lis) parametrized to support FIFO based sorter scheme, run time configurable size and descending sorter direction, has been used for lagging and leading reference cells.

The CFAR Chisel generator is described with following Scala files available inside`src/main/scala` directory:

* `CFARUtils.scala` - contains useful modules used inside CFARcore
	* `AdjustableShiftRegisterStream`  - used for guard cells, interface conform to the AXI4 stream interface (ready/valid protocol with last signal). Depth of the register is controlled by the input signal `depth`.
	* `CellUnderTest` - simple module describes behaviour of the cell under test. Interface of this module conform to the AXI4 stream interface (ready/valid protocol with last signal)
	* `ShiftRegisterMemStream` - shift register implemented with chisel object [SyncReadMem](https://www.chisel-lang.org/api/latest/chisel3/SyncReadMem.html) for mapping it to BRAM/SRAM. Used when `CFARAlgorithm` is set to `CACFARType`. Interface conform to AXI4 stream (ready/valid protocol with last signal) with additional in/out signals such as `depth`, `memFull` and `memEmpty`.
* `CFARCoreWithMem.scala` - CFAR core where CACFAR algorithm is implemented.  `ShiftRegisterMemStream` is used for leading/lagging cells
* `CFARCoreWithLis.scala` - CFAR core where both GOSCACFAR and GOSCFAR  algorithms are implemented.  `LinearSorter` is used for leading/lagging cells
* `CFARCore.scala` - contains interface of the generator and  `CFARCore` module which instantiate appropriate CFAR module checking `CFARAlgorithm` parameter
* `CFARParams.scala`- defines parameters of the CFAR generator
* `CFARDspBlock.scala`-  contains description of  the `CFARDspBlock`.

#### Inputs

[Decoupled](http://github.com/freechipsproject/chisel3/wiki/Interfaces-Bulk-Connections) interface is used where .bits are data that should be sorted.
* `in: Flipped(Decoupled(params.protoIn))` - input data  wrapped with valid/ready signals
* `lastIn: Bool()` - 	 denotes the last sample of the streaming input data
* Control registers:
	* Common registers
		* `thresholdScaler` - threshold scale factor, used to either multiply or add to the calculated noise to determine threshold for peak detection. It defines the probability of False Alarm(FA) and Missed Detection(MD)
		* `logOrLinearMode` - input data is magnitude/squared magnitude (linear mode) or log2 magnitude (log mode)
		* `peakGrouping` - one bit register used to enable or disable local max checking logic
		* `cfarAlgorithm` - only included if parameter `CFARAlgorithm`  is set to   `GOSCACFARType`  and define whether result of the `CACFAR` or `GOSCFAR` should be sent to the output
		* `cfarMode` - defines cfar mode
			* CA  - standard cell average
			* GO - greatest of
			* SO - smallest of
		* `windowCells` - number of active cells inside leading/lagging window
		* `guardCells` - number of active cells inside guard window
		* `fftWin` - present size of the fft window (it is assumed that preceding fft block supports run time configurable fft size)
	* `CA-CFAR` only specific registers
		*  `divSum`  - define division factor of the surrounding noise
	* `GOS-CFAR` only specific registers
		*  `indexLead` - define index inside sorted leading sequence used for threshold computation
		*  `indexLag` - define index inside sorted lagging sequence used for threshold computation
#### Outputs

[Decoupled](http://github.com/freechipsproject/chisel3/wiki/Interfaces-Bulk-Connections) interface is used where .bits are data that should be sent to the streaming output.
* `out: Decoupled(CFAROutFields)` - output streaming data wrapped with valid/ready signals
	*  `CFAROutField` consists of
		 * `peak: Bool` - define whether current cell under test is peak or not
		 * `cut: protoIn`  - current cell under test
		 * `threshold: protoThreshold` - current threshold value
* `lastOut: Bool` - denotes the last sample of the streaming output
*  `fftBin : UInt` - current fft bin

#### Dsp Block

The  CFAR generator is wrapped as generic DSP block in a diplomatic interface which is actually AXI4-Stream for inputs and outputs and optional memory-mapped bus (TileLink, AXI4, APB or AHB) for control and status registers. Appropriate Chisel code which does above mentioned wrapping is available inside `CFARDspBlock.scala`.

## Parameter settings

Design parameters are defined inside `case class CFARParams`. Users can customize design per use case by setting the appropriate parameters.

    case class CFARParams[T <: Data: Real](
      protoIn           : T, // Data type of the input data
      protoThreshold    : T, // Data type of the threshold
      protoScaler       : T, // Data type of the threshold scale factor
      CFARAlgorithm     : CFARType = GOSCACFARType, // CFAR algorithm
      leadLaggWindowSize: Int = 16,   // maximum number of leading/lagging cells
      guardWindowSize   : Int = 4,    // maximum number of guard cells
      fftSize           : Int = 1024, // maximum fft size which supports preceding fft block
      numAddPipes       : Int = 0,    // number of add pipeline registers
      numMulPipes       : Int = 0     // number of mull pipeline registers
      )
The further explanation of each parameter is given below:
-   `protoIn:` represents type of the input data. Users can choose among following Chisel types: `UInt`, `SInt`, `FixedPoint`, `DspReal`. Type `DspReal`is used to make golden model of the digital design.  Input data type corresponds directly to output data type of the `LogMagMux` block.
*   `protoThreshold:` is data type of threshold. Usually set to the same type value as the `protoIn`
*   `protoScaler:` represents data type of the threshold scale factor
*   `CFARAlgorithm:` CFAR algorithm
	* `CACFARType` - Cell averaging CFAR algorithm - uses BRAM/SRAM for leading/lagging cells (`ShiftRegisterMemStream` object from `CFARutil.scala` is used)
	* `GOSCFARType` - Generalised ordered statistic CFAR algorithms - uses registers for leading/lagging cells (Linear Insertion sorter is used for leading/lagging window)
	* `GOSCACFARType` - Generalised ordered statistic and standard cell averaging CFAR  algorithms - uses registers for leading/lagging cells (Linear insertion sorter is used for leading/lagging window)
	* `leadLaggWindowSize` - Maximum number of leading/lagging cells, it can be non power of 2, default value is equal to 16
	* `guardWindowSize` - Maximum number of guard cells, it can be non power of 2, default value is equal to 4
	* `fftSize` - Maximum fft size which supports preceding fft block, default value is 1024
	* `numAddPipes` - Number of pipeline registers added after +/- operation, used to pipe threshold value when log input mode is used
	* `numMulPipes` - Number of pipeline registers added after * operation, used to pipe threshold value when linear input mode is used

## Prerequisites

The following software packages should be installed prior to running this project:
* [sbt](http://www.scala-sbt.org)
* [Verilator](http://www.veripool.org/wiki/verilator)

## Setup

Clone this repository, switch directory and run tests:
```
git clone https://github.com/milovanovic/cfar.git
cd cfar
sbt test
```
## Tests

This repository provides simple tests which confirm the correct behaviour of the proposed design. Tests are described with following files available inside `src/test/scala`:
* `CFARUtilSpec` - test modules defined inside `CFARUtils.scala`
* `CFARCoreSpec` - test `CFARCore` for various test cases

Tester functions such as `peek`, `poke` and `except`, available inside `DspTester` (check [dsptools Chisel library ](http://github.com/ucb-bar/dsptools)), are extensively used for design testing.

## TODO
- Update documentation (CASH algorithm, edge handling methods)
- Implement all edge handling methods for `CFARCoreWithMem` core  and `CFARCoreWithLis`core as well
- Remove support for scala version 2.11
- Check timing when register for  edge handling method configuration is enabled
* Add more test cases
* Attach plot diagrams
