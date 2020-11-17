package magnitude

import org.scalatest.{FlatSpec, Matchers}
import chisel3.experimental.FixedPoint
import dsptools.DspContext

import breeze.math._
import breeze.linalg.randomDouble

import scala.math._
import scala.util.Random

class LogMagMuxSpec extends FlatSpec with Matchers {
 // def test_setup(base_params: MAGParams[FixedPoint], numSamples: Int): (Seq[Complex], Seq[Seq[Complex]]) = {
  def test_setup(numSamples: Int): (Seq[Complex], Seq[Seq[Complex]]) = {
    // define seed here!
    // define data which goes from zero to some kind of value
    val inp = Seq.fill(numSamples) { Complex((randomDouble() * 2 - 1) * 50, (randomDouble() * 2 - 1) * 50) }
    val out : Seq[Seq[Complex]] = inp.map {
        case e => {
          val u = abs(e.real).max(abs(e.imag))
          val v = abs(e.real).min(abs(e.imag))
          val jpl = (u + v/8).max(7 * u/8 + v/2)
          val magSqr = e.real * e.real + e.imag * e.imag
          Seq(e, Complex(magSqr, 0.0), Complex(jpl, 0.0), Complex(log(jpl)/log(2), 0.0))
        }
    }.toSeq
    (inp, out)
  }

  behavior of "LogMagMux"
  val (inp, out) = test_setup(numSamples = 10)

  // this test should test log2 of integer data
  it should f"test log max mux with binaryPoint set to zero" in {
    val base_params = MAGParams.fixed(
      dataWidth = 16,
      binPoint = 0,
      dataWidthLog = 16,
      binPointLog = 11,
      log2LookUpWidth = 11,
      numAddPipes = 1,
      numMulPipes = 1,
      useLast = true
    )
    val numSamples = 10
    Random.setSeed(11110L) // generate always the same test example

    val randomReal = Seq.fill(numSamples)((Random.nextInt((1 << (16/2-1))*2) - (1 << (16/2-1))).toInt)
    val randomImag = Seq.fill(numSamples)((Random.nextInt((1 << (16/2-1))*2) - (1 << (16/2-1))).toInt)
    
    val inpInt = randomReal.zip(randomImag).map { case (real, imag) => Complex(real, imag)}
    val outInt : Seq[Seq[Complex]] = inpInt.map {
        case e => {
          val u = abs(e.real).max(abs(e.imag))
          val v = abs(e.real).min(abs(e.imag))
          val jpl = (u + v/8).max(7 * u/8 + v/2)
          // do not care about mag squared
          val magSqr = e.real * e.real + e.imag * e.imag
          Seq(e, Complex(magSqr, 0.0), Complex(jpl, 0.0), Complex(log(jpl)/log(2), 0.0))
        }
    }.toSeq
    FixedMagTester(base_params, inpInt, outInt) should be (true)
  }

  it should f"test log magnitude mux with no pipeline registers" in {
    val base_params = MAGParams.fixed(
      dataWidth = 24,
      binPoint = 10,
      dataWidthLog = 24,
      binPointLog = 10,
      log2LookUpWidth = 10,
      numAddPipes = 0,
      numMulPipes = 0
    )
    FixedMagTester(base_params, inp, out) should be (true)
  }
  
  it should f"test log magnitude mux with numAddPipes = 1 and numMulPipes = 1" in {
    val base_params = MAGParams.fixed(
      dataWidth = 24,
      binPoint = 10,
      dataWidthLog = 24,
      binPointLog = 10,
      log2LookUpWidth = 10,
      numAddPipes = 1,
      numMulPipes = 1
    )
    FixedMagTester(base_params, inp, out) should be (true)
  }
  
  it should f"test JPL stream calculation with numAddPipes = 0 and numMulPipes = 0 and included lastIn and lastOut signals " in {
    val base_params = MAGParams.fixed(
      dataWidth = 24,
      binPoint = 10,
      dataWidthLog = 24,
      binPointLog = 10,
      log2LookUpWidth = 10,
      useLast = true,
      numAddPipes = 0,
      numMulPipes = 0
    )
    val outJPL : Seq[Complex] = out.map(c => c(2))
                          // JPL  //sel
    FixedMagTester(base_params, inp, outJPL, 2) should be (true)
  }
  
  it should f"test JPL stream calculation with numAddPipes = 1 and numMulPipes = 1 and included lastIn and lastOut signals" in {
    val base_params = MAGParams.fixed(
      dataWidth = 24,
      binPoint = 10,
      dataWidthLog = 24,
      binPointLog = 10,
      log2LookUpWidth = 10,
      numAddPipes = 1,
      numMulPipes = 1,
      useLast = true
    )
    val outJPL : Seq[Complex] = out.map(c => c(2))
                         // JPL  //sel
    FixedMagTester(base_params, inp, outJPL, 2) should be (true)
  }
}

