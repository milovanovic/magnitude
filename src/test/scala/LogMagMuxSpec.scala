// SPDX-License-Identifier: Apache-2.0

package magnitude

import chisel3.{fromDoubleToLiteral => _, fromIntToBinaryPoint => _, _}
import chisel3.util._
import fixedpoint._
import dsptools.DspContext

import breeze.math._
import breeze.linalg.randomDouble
import breeze.stats.distributions.RandBasis
import scala.math._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest._

class LogMagMuxSpec extends AnyFlatSpec with ChiselScalatestTester {
  val r = scala.util.Random
  def test_setup(numSamples: Int): (Seq[Complex], Seq[Seq[Double]]) = {
    val inp = Seq.fill(numSamples) { Complex((r.nextDouble() * 2 - 1) * 50, (r.nextDouble() * 2 - 1) * 50) }
    val out: Seq[Seq[Double]] = inp.map {
      case e => {
        val u = abs(e.real).max(abs(e.imag))
        val v = abs(e.real).min(abs(e.imag))
        val jpl = (u + v / 8).max(7 * u / 8 + v / 2)
        val magSqr = e.real * e.real + e.imag * e.imag
        Seq(magSqr, jpl, log(jpl) / log(2))
      }
    }.toSeq
    (inp, out)
  }

  behavior.of("LogMagMux")
  val (inp, out) = test_setup(numSamples = 10)

  it should f"test LogMagMux module, changing sel signal and no pipeline registers" in {
    val base_params: MAGParams[FixedPoint] = MAGParams(
      protoIn = FixedPoint(24.W, 10.BP),
      protoOut = FixedPoint(24.W, 10.BP),
      protoLog = Some(FixedPoint(24.W, 10.BP)),
      magType = LogMagMux,
      useLast = true,
      log2LookUpWidth = 10,
      numAddPipes = 0,
      numMulPipes = 0
    )
    //FixedMagTester(base_params, inp, out) //should be(true)
    test(new LogMagMuxGenerator(base_params))
      .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation))
      .runPeekPoke(new LogMagMuxTester(_) {
        testMux(inp, out)
      })
  }
//
//  // IMPORTANT TODO: use trim squared magnitude and adjust tests according to that
//  it should f"test LogMagMux module, changing sel signal and numAddPipes = 1, numMulPipes = 1 " in {
//    val base_params: MAGParams[FixedPoint] = MAGParams(
//      protoIn = FixedPoint(24.W, 10.BP),
//      protoOut = FixedPoint(24.W, 10.BP),
//      protoLog = Some(FixedPoint(24.W, 10.BP)),
//      magType = LogMagMux,
//      useLast = true,
//      log2LookUpWidth = 10,
//      numAddPipes = 1,
//      numMulPipes = 1
//    )
//    FixedMagTester(base_params, inp, out) //should be(true)
//  }
  for (numPipes <- Seq(Seq(0, 0), Seq(1, 1))) {
    for (sel <- Seq(0, 1, 2)) {
      val addPipes = numPipes(0)
      val mulPipes = numPipes(1)
      it should f"test LogMagMux module, stream calculation with numAddPipes = $addPipes and numMulPipes = $mulPipes and included lastIn and lastOut signals, sel signal is equal to $sel" in {
        val base_params: MAGParams[FixedPoint] = MAGParams(
          protoIn = FixedPoint(24.W, 10.BP),
          protoOut = FixedPoint(24.W, 10.BP),
          protoLog = Some(FixedPoint(24.W, 10.BP)),
          magType = LogMagMux,
          useLast = true,
          log2LookUpWidth = 10,
          numAddPipes = addPipes,
          numMulPipes = mulPipes
        )
        val outLogMag: Seq[Double] = out.map(c => c(sel))
        // tolerance is 6 because of squared magnitude calculation
        test(new LogMagMuxGenerator(base_params))
          .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation))
          .runPeekPoke(new LogMagMuxTester(_) {
            testStream(inp, sel, outLogMag)
          })
        //        FixedMagTester(base_params, inp, outLogMag, sel, 8) should be(true)
      }
    }
  }

  for (numAddPipes <- Seq(0, 1, 2)) {
    it should f"test MagJPL module, stream calculation with numAddPipes = $numAddPipes and included lastIn and lastOut signals" in {
      val base_params: MAGParams[FixedPoint] = MAGParams(
        protoIn = FixedPoint(24.W, 10.BP),
        protoOut = FixedPoint(24.W, 10.BP),
        protoLog = Some(FixedPoint(24.W, 10.BP)),
        magType = MagJPL,
        useLast = true,
        log2LookUpWidth = 10,
        numAddPipes = numAddPipes
      )
      val outJPL: Seq[Double] = out.map(c => c(1))
      test(new LogMagMuxGenerator(base_params))
        .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation))
        .runPeekPoke(new LogMagMuxTester(_) {
          testStream(inp, 0, outJPL)
        })
      //FixedMagTester(base_params, inp, outJPL, 0) should be(true)
    }
  }

  for (numAddPipes <- Seq(0, 1, 2)) {
    it should f"test LogMag module, stream calculation with numAddPipes = $numAddPipes and included lastIn and lastOut signals" in {
      val base_params: MAGParams[FixedPoint] = MAGParams(
        protoIn = FixedPoint(24.W, 10.BP),
        protoOut = FixedPoint(24.W, 10.BP),
        protoLog = Some(FixedPoint(24.W, 10.BP)),
        magType = LogMag,
        useLast = true,
        log2LookUpWidth = 10,
        numAddPipes = numAddPipes
      )
      val outLogMag: Seq[Double] = out.map(c => c(2))
      test(new LogMagMuxGenerator(base_params))
        .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation))
        .runPeekPoke(new LogMagMuxTester(_) {
          testStream(inp, 0, outLogMag) // default tolerance is 3
        })
      //FixedMagTester(base_params, inp, outLogMag, 0) should be(true)
    }
  }

  for (numAddPipes <- Seq(0, 1, 2)) {
    for ((sel, ind) <- Seq(1, 2).zipWithIndex) {
      it should f"test MagJPLandLogMag module, stream calculation with numAddPipes = $numAddPipes and included lastIn and lastOut signals, sel signal is equal to $ind" in {
        val base_params: MAGParams[FixedPoint] = MAGParams(
          protoIn = FixedPoint(24.W, 10.BP),
          protoOut = FixedPoint(24.W, 10.BP),
          protoLog = Some(FixedPoint(24.W, 10.BP)),
          magType = MagJPLandLogMag,
          useLast = true,
          log2LookUpWidth = 10,
          numAddPipes = numAddPipes
        )
        val outMagJPLandLogMag: Seq[Double] = out.map(c => c(sel))
        test(new LogMagMuxGenerator(base_params))
          .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation))
          .runPeekPoke(new LogMagMuxTester(_) {
            testStream(inp, ind, outMagJPLandLogMag)
          })
        // FixedMagTester(base_params, inp, outMagJPLandLogMag, ind) should be(true)
      }
    }
  }

  for (numPipes <- Seq(Seq(0, 0), Seq(1, 1))) {
    for (sel <- Seq(0, 1)) {
      val addPipes = numPipes(0)
      val mulPipes = numPipes(1)
      it should f"test MagJPLandSqrMag module, stream calculation with numAddPipes = $addPipes and numMulPipes = $mulPipes and included lastIn and lastOut signals, sel signal is equal to $sel" in {
        val base_params: MAGParams[FixedPoint] = MAGParams(
          protoIn = FixedPoint(24.W, 10.BP),
          protoOut = FixedPoint(24.W, 10.BP),
          protoLog = Some(FixedPoint(24.W, 10.BP)),
          magType = MagJPLandSqrMag,
          useLast = true,
          log2LookUpWidth = 10,
          numAddPipes = addPipes,
          numMulPipes = mulPipes
        )
        val outMagJPLandSqrMag: Seq[Double] = out.map(c => c(sel))
        test(new LogMagMuxGenerator(base_params))
          .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation))
          .runPeekPoke(new LogMagMuxTester(_) {
            testStream(inp, sel, outMagJPLandSqrMag, 6)
          })
        //FixedMagTester(base_params, inp, outMagJPLandSqrMag, sel, 8) should be(true)
      }
    }
  }
}
