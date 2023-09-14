// SPDX-License-Identifier: Apache-2.0

package magnitude

import chisel3.{fromDoubleToLiteral => _, fromIntToBinaryPoint => _, _}
import chisel3.util._
import fixedpoint._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

import dsptools.numbers._
import dspblocks._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import org.chipsalliance.cde.config.Parameters

import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._

// make standalone block for LogMagMux
trait AXI4StreamMultipleMagBlocksStandaloneBlock extends AXI4StreamMultipleMagBlocks[FixedPoint] {
  def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
  val ioMem = mem.map { m =>
    {
      val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))

      m :=
        BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) :=
        ioMemNode

      val ioMem = InModuleBody { ioMemNode.makeIO() }
      ioMem
    }
  }

  val numIns = 4

  val ins: Seq[ModuleValue[AXI4StreamBundle]] = for (i <- 0 until numIns) yield {
    implicit val valName = ValName(s"in_$i")
    val in = BundleBridgeSource[AXI4StreamBundle](() => AXI4StreamBundle(AXI4StreamBundleParameters(n = 4)))
    streamNode :=
      BundleBridgeToAXI4Stream(AXI4StreamMasterPortParameters(AXI4StreamMasterParameters(n = 4))) :=
      in
    InModuleBody { in.makeIO() }
  }
  val outs: Seq[ModuleValue[AXI4StreamBundle]] = for (o <- 0 until numIns) yield {
    implicit val valName = ValName(s"out_$o")
    val out = BundleBridgeSink[AXI4StreamBundle]()
    out :=
      AXI4StreamToBundleBridge(AXI4StreamSlavePortParameters(AXI4StreamSlaveParameters())) :=
      streamNode
    InModuleBody { out.makeIO() }
  }
}

abstract class MultipleMagBlocks[T <: Data: Real: BinaryRepresentation, D, U, E, O, B <: Data](
  params:    MAGParams[T],
  beatBytes: Int)
    extends LazyModule()(Parameters.empty)
    with DspBlock[D, U, E, O, B]
    with HasCSR {

  // this block requires that sel signal is there, no HasCSR when JPL only or Mag Sqr only or LogMag only is set
  require(params.magType == MagJPLandSqrMag || params.magType == MagJPLandLogMag || params.magType == LogMagMux)

  val dataWidthOut = params.protoOut.getWidth

  val streamNode = AXI4StreamNexusNode(
    masterFn =
      (ms: Seq[AXI4StreamMasterPortParameters]) => AXI4StreamMasterPortParameters(ms.map(_.masters).reduce(_ ++ _)),
    slaveFn = ss => {
      AXI4StreamSlavePortParameters(ss.map(_.slaves).reduce(_ ++ _))
    }
  )
  val slaveParams = AXI4StreamSlaveParameters()

  lazy val module = new LazyModuleImp(this) {
    val (ins, _) = streamNode.in.unzip
    val (outs, _) = streamNode.out.unzip

    // if it is only one type then this register should not be considered at all
    val selRegDataWidth = if (params.magType == LogMagMux) 2 else 1

    val selReg = RegInit(0.U(selRegDataWidth.W))
    val fields = Seq(
      RegField(
        selRegDataWidth,
        selReg,
        RegFieldDesc(name = "sel", desc = "selection signal for the log magnitude multiplexer")
      )
      // left for future addition
    )

    // Define abstract register map so it can be AXI4, Tilelink, APB, AHB
    regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f) }): _*)

    for ((in, inIdx) <- ins.zipWithIndex) {
      //  Log magnitude mux module
      val logMagMux = Module(new LogMagMuxGenerator(params))
      logMagMux.io.sel.get := selReg

      // Connect inputs
      logMagMux.io.in.valid := in.valid
      logMagMux.io.in.bits := in.bits.data.asTypeOf(DspComplex(params.protoIn))
      in.ready := logMagMux.io.in.ready
      if (params.useLast) {
        logMagMux.io.lastIn.get := in.bits.last
      }

      // Connect outputs
      outs(inIdx).valid := logMagMux.io.out.valid
      logMagMux.io.out.ready := outs(inIdx).ready
      outs(inIdx).bits.data := logMagMux.io.out.bits.asUInt
      if (params.useLast) {
        outs(inIdx).bits.last := logMagMux.io.lastOut.get
      }
    }
  }
}

class AXI4StreamMultipleMagBlocks[T <: Data: Real: BinaryRepresentation](
  params:     MAGParams[T],
  address:    AddressSet,
  _beatBytes: Int = 4
)(
  implicit p: Parameters)
    extends MultipleMagBlocks[
      T,
      AXI4MasterPortParameters,
      AXI4SlavePortParameters,
      AXI4EdgeParameters,
      AXI4EdgeParameters,
      AXI4Bundle
    ](params, _beatBytes)
    with AXI4DspBlock
    with AXI4HasCSR {
  override val mem = Some(AXI4RegisterNode(address = address, beatBytes = _beatBytes))
}

object MultipleMagBlocksApp extends App {
  // here just define parameters
  val params: MAGParams[FixedPoint] = MAGParams(
    protoIn = FixedPoint(16.W, 8.BP),
    protoOut = FixedPoint(20.W, 8.BP),
    protoLog = Some(FixedPoint(16.W, 8.BP)),
    magType = MagJPLandLogMag,
    log2LookUpWidth = 8,
    useLast = true,
    numAddPipes = 1,
    numMulPipes = 1
  )

  val baseAddress = 0x500
  implicit val p: Parameters = Parameters.empty

  val lazyDut = LazyModule(
    new AXI4StreamMultipleMagBlocks(params, AddressSet(baseAddress + 0x100, 0xff), _beatBytes = 4)
      with AXI4StreamMultipleMagBlocksStandaloneBlock
  )
  (new ChiselStage).execute(
    Array("--target-dir", "verilog/AXI4StreamMultipleMagBlocks"),
    Seq(ChiselGeneratorAnnotation(() => lazyDut.module))
  )
}
