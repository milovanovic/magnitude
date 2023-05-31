// SPDX-License-Identifier: Apache-2.0

package magnitude

import chisel3._
import chisel3.experimental._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

import dsptools.numbers._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.diplomacy._

import org.chipsalliance.cde.config.Parameters

// make standalone block for LogMagMux
trait AXI4StreamMultipleSimpleMagBlocksStandaloneBlock extends AXI4StreamMultipleSimpleMagBlocks[FixedPoint] {

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

class AXI4StreamMultipleSimpleMagBlocks[T <: Data: Real: BinaryRepresentation](params: MAGParams[T])
    extends LazyModule()(Parameters.empty) {

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

    for ((in, inIdx) <- ins.zipWithIndex) {
      //  Log magnitude mux module
      val logMagMux = Module(new LogMagMuxGenerator(params))

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

object MultipleSimpleMagBlocksApp extends App {
  // here just define parameters
  val params: MAGParams[FixedPoint] = MAGParams(
    protoIn = FixedPoint(16.W, 8.BP),
    protoOut = FixedPoint(20.W, 8.BP),
    magType = MagJPL,
    useLast = true,
    numAddPipes = 1,
    numMulPipes = 1
  )

  val baseAddress = 0x500
  implicit val p: Parameters = Parameters.empty

  val lazyDut = LazyModule(
    new AXI4StreamMultipleSimpleMagBlocks(params) with AXI4StreamMultipleSimpleMagBlocksStandaloneBlock
  )
  (new ChiselStage).execute(
    Array("--target-dir", "verilog/AXI4StreamMultipleSimpleMagBlocks"),
    Seq(ChiselGeneratorAnnotation(() => lazyDut.module))
  )
}
