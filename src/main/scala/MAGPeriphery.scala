// SPDX-License-Identifier: Apache-2.0

package magnitude

import chisel3._
import chisel3.util._
import chisel3.experimental.{DataMirror, FixedPoint}

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.{Config, Field}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.BaseSubsystem

import dsptools.numbers._

/* MAG parameters and addresses */
case class MAGParamsAndAddress[T <: Data: Real: BinaryRepresentation](
  magParams:  MAGParams[T],
  magAddress: AddressSet,
  useAXI4:    Boolean)

/* AXI4MAG FixedPoint Key */
case object MAGKey extends Field[Option[MAGParamsAndAddress[FixedPoint]]](None)

trait CanHavePeripheryMAG { this: BaseSubsystem =>
  private val portName = "mag"

  val mag = p(MAGKey) match {
    case Some(params) => {
      val mag = if (params.useAXI4) {
        val mag = LazyModule(
          new AXI4LogMagMuxBlock(
            address = params.magAddress,
            params = params.magParams,
            _beatBytes = pbus.beatBytes
          )
        )
        // Connect mem
        pbus.coupleTo("mag") {
          mag.mem.get := AXI4Buffer() := TLToAXI4() := TLFragmenter(
            pbus.beatBytes,
            pbus.blockBytes,
            holdFirstDeny = true
          ) := _
        }
        // return
        Some(mag)
      } else {
        val mag = LazyModule(
          new TLLogMagMuxBlock(
            address = params.magAddress,
            params = params.magParams,
            beatBytes = pbus.beatBytes
          )
        )
        // Connect mem
        pbus.coupleTo("mag") { mag.mem.get := TLFragmenter(pbus.beatBytes, pbus.blockBytes) := _ }
        // return
        Some(mag)
      }
      // streamNode
      val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = pbus.beatBytes)))
      val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()

      ioOutNode := AXI4StreamToBundleBridge(
        AXI4StreamSlaveParameters()
      ) := mag.get.streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = pbus.beatBytes)) := ioInNode

      val mag_in = InModuleBody { ioInNode.makeIO() }
      val mag_out = InModuleBody { ioOutNode.makeIO() }

      // return
      Some(Seq(mag_in, mag_out))
    }
    case None => None
  }
}

trait CanHavePeripheryMAGModuleImp extends LazyModuleImp {
  val outer: CanHavePeripheryMAG
}

class MAGMirrorIO[T <: Data](private val gen1: T, private val gen2: T) extends Bundle {
  val in = DataMirror.internal.chiselTypeClone[T](gen1)
  val out = Flipped(DataMirror.internal.chiselTypeClone[T](gen2))
}

/* Mixin to add AXI4MAG to rocket config */
class WithMAG(magParams: MAGParams[UInt], magAddress: AddressSet = AddressSet(0x3000, 0xff), useAXI4: Boolean)
    extends Config((site, here, up) => {
      case MAGKey =>
        Some(
          (MAGParamsAndAddress(
            magParams = magParams,
            magAddress = magAddress,
            useAXI4 = useAXI4
          ))
        )
    })

case object MAGAdapter {
  def tieoff(mag: Option[MAGMirrorIO[AXI4StreamBundle]]): Unit = {
    mag.foreach { s =>
      s.in.valid := false.B
      s.in.bits := DontCare
      s.out.ready := true.B
    }
  }

  def tieoff(mag: MAGMirrorIO[AXI4StreamBundle]): Unit = { tieoff(Some(mag)) }
}
