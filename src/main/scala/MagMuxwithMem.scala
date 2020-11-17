package magnitude

import chisel3._
import chisel3.util._
import chisel3.experimental._//.FixedPoint

import dsptools._
import dsptools.numbers._

import breeze.numerics.{cos, sin}
import scala.math.{Pi, pow}
import breeze.math.Complex

class LogMagMuxMem [T <: Data : Real : BinaryRepresentation](val params: MAGParams[T]) extends Module {
  val io = IO(new Bundle {
    val magEnable    = Input(Bool())
    val magReadyOut  = Input(Bool())
    val sel          = Input(UInt(2.W))
    val magValidOut = Output(Bool())
    val out = Output(params.proto)
  })
  val inp = Wire(params.proto.cloneType)
   
  val mag = Module(new LogMagMux(params))
  mag.io.out.ready := io.magReadyOut
  
  mag.io.in.valid := RegNext(io.magEnable,false.B)
  mag.io.sel := io.sel
    
  val rstProtoIQ = Wire(params.proto.cloneType)
  rstProtoIQ.real:= Real[T].fromDouble(0.0)
  rstProtoIQ.imag:= Real[T].fromDouble(0.0)

  val mag_InputData = Wire(Vec(1024, params.proto.cloneType))
  val inputDataReg = RegInit(rstProtoIQ)

  val numSamples = 1024
  val f1 = 0.04
  val f2 = 0.1
  
  // just to have something written in memory it doesn't matter what it is exactly
  val testToneDouble = (0 until numSamples).map(i => Complex(((math.sin(2 * math.Pi * f1 * i) + math.sin(2 * math.Pi * f2 * i))*((math.pow(2,14)-1))), math.sin(2 * math.Pi * f1)*((math.pow(2,14)-1)))).toVector

  (0 until 1024).map(n => {
    mag_InputData(n).real := Real[T].fromDouble(testToneDouble(n).real)
    mag_InputData(n).imag := Real[T].fromDouble(testToneDouble(n).imag)
  })
  val cntr = RegInit(0.U(log2Up(1024).W))
  
  when (io.magEnable) {
    cntr := cntr + 1.U
  }
  inputDataReg := mag_InputData(cntr)
  inp := inputDataReg 
  val ready = mag.io.in.ready
  
  mag.io.out.ready := true.B
  mag.io.in.bits := inp
  io.out := mag.io.out.bits
  
  io.magValidOut := mag.io.out.valid
}

object LogMagMuxModule extends App
{
//   val params = MAGParams.fixed(
//     dataWidth = 16,
//     binPoint = 0,
//     dataWidthLog = 16,
//     binPointLog = 8,
//     log2LookUpWidth = 7,
//     numAddPipes = 0,
//     numMulPipes = 0
//   )
//   val params = MAGParams.fixed(
//     dataWidth = 16,
//     binPoint = 10,
//     dataWidthLog = 16,
//     binPointLog = 10,
//     log2LookUpWidth = 10,
//     numAddPipes = 0,
//     numMulPipes = 0
//   )
  val params = MAGParams.fixed(
    dataWidth = 16,
    binPoint = 10,
    dataWidthLog = 16,
    binPointLog = 10,
    log2LookUpWidth = 10,
    numAddPipes = 1,
    numMulPipes = 1
  )
  chisel3.Driver.execute(args,()=>new LogMagMuxMem(params))
}

