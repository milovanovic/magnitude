package magnitude

import chisel3._
import chisel3.util._
import chisel3.experimental._//FixedPoint

import dsptools._
import dsptools.numbers._

import scala.math._

case class MAGParams[T <: Data] (
  val proto           : DspComplex[T], // width of input and output
  val protoLog        : T,
  val log2LookUpWidth : Int, // number of bits that are input address at look_up_table
  val useLast         : Boolean, // use lastIn and lastOut AXI signals
  val numAddPipes     : Int, // number of pipeline registers after + operation
  val numMulPipes     : Int // number of pipeline registers after - operation
) {
  requireIsChiselType(proto)
  // ... to continue
}

// Here we should add special dataType for the log2LookUp output
// constrain that output that must fit to 
object MAGParams {
  def fixed(dataWidth        : Int = 16,
            binPoint         : Int = 14,
            dataWidthLog     : Int = 16,
            binPointLog      : Int = 10,
            log2LookUpWidth  : Int = 16,
            useLast          : Boolean = false,
            numAddPipes      : Int = 1,
            numMulPipes      : Int = 1
            ): MAGParams[FixedPoint] = {
    val proto = DspComplex(FixedPoint(dataWidth.W, binPoint.BP))
    val protoLog = FixedPoint(dataWidthLog.W, binPointLog.BP)
    MAGParams(
      proto = proto,
      protoLog = protoLog,
      log2LookUpWidth = log2LookUpWidth,
      useLast = useLast,
      numAddPipes = numAddPipes,
      numMulPipes = numMulPipes
    )
  }
}

/**
 *  LogMagMux block computes power of the input signal (squared magnitude), magnitude using JPL aproximation 
 *  and log2 magnitude. Also it can bypass input data depending on selection input signal.
 */
class MagMuxIO[T <: Data: Real](val params: MAGParams[T]) extends Bundle {
  val in = Flipped(Decoupled(params.proto))
  val lastIn = if (params.useLast) Some(Input(Bool())) else None
  val out = Decoupled(params.proto)
  val sel = Input(UInt(2.W))
  val lastOut = if (params.useLast) Some(Output(Bool())) else None
  
  override def cloneType: this.type = MagMuxIO(params).asInstanceOf[this.type]
}
object MagMuxIO {
  def apply[T <: Data : Real](params: MAGParams[T]): MagMuxIO[T] = new MagMuxIO(params)
}

class LogMagMux[T <: Data: Real : BinaryRepresentation](val params: MAGParams[T]) extends Module {
  
  val io = IO(MagMuxIO(params))
  
  val gen = params.proto.real
  val dataWidth = gen.getWidth
  val bpos = (gen match {
    case fp: FixedPoint => fp.binaryPoint.get
    case _ => 0
  })

  val bposLog = (params.protoLog match {
    case fp: FixedPoint => fp.binaryPoint.get
    case _ => 0
  })
  val dataWidthLog = params.protoLog.getWidth
  
  val numAddPipes = params.numAddPipes
  val numMulPipes = params.numMulPipes
  val magSqrLatency = numAddPipes + numMulPipes
  val jplMagLatency = 2 * numAddPipes
  val log2Latency = jplMagLatency + numAddPipes  + numAddPipes //added + numAddPipes for address generation
  val latency = magSqrLatency.max(log2Latency)
 
  val absInReal = Real[T].abs(io.in.bits.real)
  val absInImag = Real[T].abs(io.in.bits.imag)
  // for JPL approximation
  val u = Real[T].max(absInReal, absInImag)
  val v = Real[T].min(absInReal, absInImag)
  val magSqr = Wire(params.proto.real)
  
  val jplMagOp1 = DspContext.withNumAddPipes(2*numAddPipes) { // to be aligned with jplMagOp2
                            u context_+ BinaryRepresentation[T].shr(v,3) } // U + V/8
  val tmpOp2 = DspContext.withNumAddPipes(numAddPipes) { u context_- BinaryRepresentation[T].shr(u,3) }
  
  val jplMagOp2 = DspContext.withNumAddPipes(numAddPipes) {
                            tmpOp2 context_+  ShiftRegister(BinaryRepresentation[T].shr(v, 1), numAddPipes, true.B) }  // (7/8)*U + 1/2*V
  val last = RegInit(false.B)
  
  magSqr := DspContext.alter(DspContext.current.copy(numAddPipes = numAddPipes,  numMulPipes = numMulPipes)) {
                       ShiftRegister((absInReal context_* absInReal) context_+ (absInImag context_* absInImag), latency - magSqrLatency, true.B)}
  
  val jplMagtmp = Real[T].max(jplMagOp1, jplMagOp2)
  val jplMag = ShiftRegister(jplMagtmp, latency - jplMagLatency, true.B)
  // log(N) = k + log2(1 + f)
  // N = 2^k(1 + f)

  val logUInt = Log2(jplMag.asUInt()) // log2(jplMag.asUInt()(dataWidth-1, binPoint-1))
  val logSInt = (logUInt - bpos.U).asSInt()
  
  val logLookUp = VecInit({
    val lnOf2 = scala.math.log(2)                                // natural log of 2
    def log2(x: Double): Double = scala.math.log(x) / lnOf2      // function log of 2
    val num = 1 << (params.log2LookUpWidth)                      // number of look-up table values
    val lookup = (0 until num).map(n => {
      val lookupWire = Wire(FixedPoint((bposLog + 1).W, bposLog.BP)) // +2?
      // log2(1+f)
      DspContext.withTrimType(Convergent) {
        lookupWire := lookupWire.cloneType.fromDoubleWithFixedWidth(log2(n.toDouble/num + 1))
      }
      lookupWire })
    lookup
  })
  // dsptools.DspException: OverflowType Grow is not supported for UInt subtraction
  // eliminate leading 1 - think about >> <<
  val noLeadOne = ShiftRegister((jplMagtmp.asUInt() - BinaryRepresentation[UInt].shl(1.U, logUInt).asTypeOf(jplMagtmp.asUInt)), numAddPipes, true.B)
  val shiftNum = ShiftRegister(dataWidth.U - logUInt - 1.U, numAddPipes, true.B)
  
  val logLookUpAddr = BinaryRepresentation[UInt].shl(noLeadOne, shiftNum)(dataWidth - 2, dataWidth - params.log2LookUpWidth - 1)
  val logFrac = Wire(FixedPoint((bposLog+1).W, bposLog.BP))
  logFrac := logLookUp(logLookUpAddr)
//   dontTouch(logFracLog)
//   logFrac.suggestName("logFracLog")
//   
  val log2Mag = Wire(FixedPoint(dataWidthLog.W, bposLog.BP))
  log2Mag := ShiftRegister(ShiftRegister(logSInt.asFixedPoint(0.BP), numAddPipes, true.B) + logFrac, latency  - log2Latency, true.B)
//   dontTouch(log2Mag)
//   log2Mag.suggestName("log2Mag")
//   
  val magSqrOut = Wire(params.proto)
  magSqrOut.real := magSqr
  magSqrOut.imag := Real[T].fromDouble(0.0)
  
  val jplMagOut = Wire(params.proto)
  jplMagOut.real := jplMag
  jplMagOut.imag := Real[T].fromDouble(0.0)
  
  val log2MagOut = Wire(params.proto)
  log2MagOut.real := log2Mag.asTypeOf(params.proto.real)
  log2MagOut.imag := Real[T].fromDouble(0.0)

  val output = MuxLookup(ShiftRegister(io.sel, latency, en = true.B), io.in.bits,
                      Array(0.U -> ShiftRegister(io.in.bits, latency, true.B),
                            1.U -> magSqrOut,
                            2.U -> jplMagOut,
                            3.U -> log2MagOut))
  dontTouch(output)
  output.suggestName("output_mux")
  
  when (io.lastIn.getOrElse(false.B) && io.in.fire()) {
    last := true.B
  }
  val cntOutLatency = RegInit(0.U(log2Up(latency + 1).W))

  if (latency != 0) {
    val lastReady = io.out.ready && last
    when (lastReady) {
      cntOutLatency := cntOutLatency + 1.U
    }
    when (cntOutLatency === (latency.U - 1.U)) {
      last := false.B
      cntOutLatency := 0.U
    }
  }
  
  val initialInDone = RegInit(false.B)

  if (latency != 0) {
    val cntIn = RegInit(0.U(log2Ceil(latency + 1).W))
    
    when (io.in.fire()) {
      cntIn := cntIn + 1.U
    }
    
    when ((cntIn === (latency - 1).U) && io.in.fire()) {
      initialInDone := true.B
    }
    when (io.lastOut.getOrElse(false.B)) {
      initialInDone := false.B
      cntIn := 0.U
    }
  }
  
  val skidInData = Wire(io.out.cloneType)
  skidInData.bits := output
  skidInData.valid := io.in.valid
  dontTouch(skidInData)
  skidInData.suggestName("skidInData")
  io.in.ready := skidInData.ready
  Skid(latency, skidInData, io.out, initialInDone) := output

  if (params.useLast) {
    val skidInLast = Wire(Flipped(DecoupledIO(Bool())))
    val skidOutLast = Wire(DecoupledIO(Bool()))
    skidOutLast.ready := io.out.ready
    skidInLast.bits := ShiftRegister(io.lastIn.get && io.in.fire(), latency, true.B)
    skidInLast.valid := io.in.valid
    Skid(latency, skidInLast, skidOutLast, initialInDone) := skidInLast.bits
    io.lastOut.get := skidOutLast.bits
  }
}

object LogMagMuxApp extends App
{

  val params = MAGParams.fixed(
    dataWidth = 24,
    binPoint = 10,
    dataWidthLog = 24,
    //binPointLog can be > than log2LookUpWidth, but log2LookUpWidth can not be > than binPointLog
    binPointLog = 15,
    log2LookUpWidth = 10,
    numAddPipes = 1,
    numMulPipes = 1,
    useLast = true
  )
  chisel3.Driver.execute(args,()=>new LogMagMux(params))
}


