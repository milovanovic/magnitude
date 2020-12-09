// SPDX-License-Identifier: Apache-2.0

package magnitude

import dsptools.{DspTester, DspContext}
import chisel3.experimental.FixedPoint
import chisel3.iotesters.Driver
//import scala.util.Random

import breeze.math.Complex

class LogMagMuxTester[T <: chisel3.Data](dut: LogMagMux[T]) extends DspTester(dut) {
 // require(DspContext.current.numAddPipes == 0 && DspContext.current.numMulPipes == 0)
  def testMux(inp: Seq[Complex], out: Seq[Seq[Complex]], tolLSBs: Int = 3) {
    val numAddPipes = dut.params.numAddPipes
    val numMulPipes = dut.params.numMulPipes

    val magSqrLatency = numAddPipes + numMulPipes
    val jplMagLatency = 2 * numAddPipes
    val log2Latency = jplMagLatency + numAddPipes + numAddPipes
    
    val latency = magSqrLatency.max(log2Latency)

    val outComplex : Seq[Seq[Complex]] = out
    poke(dut.io.in.valid, 1)
    poke(dut.io.out.ready, 1)
    
    val genIn = dut.params.proto.real
    val genLog = dut.params.protoLog
    
    val dataWidth = genIn.getWidth
    val bposIn = (genIn match {
      case fp: FixedPoint => fp.binaryPoint.get
      case _ => 0
    })
    
    val bposLog = (genLog match {
      case fp: FixedPoint => fp.binaryPoint.get
      case _ => 0
    })
    
    val mulLog = if (bposIn < bposLog) math.pow(2, bposLog - bposIn) else 1
    
    outComplex.zipWithIndex.foreach { case (value, index) =>
      poke(dut.io.in.bits, inp(index))
      poke(dut.io.sel, 0)
      step(latency + 1) //step latency + 1
      println("Bypass mode:")
      fixTolLSBs.withValue(tolLSBs) { expect(dut.io.out.bits, value(0)) }

      println("Magnitude squared:")
      poke(dut.io.sel, 1)
      step(latency + 1)
      fixTolLSBs.withValue(7) { expect(dut.io.out.bits, value(1)) }
      
      println("JPL magnitude:") 
      poke(dut.io.sel, 2)
      step(latency + 1)
      fixTolLSBs.withValue(tolLSBs) { expect(dut.io.out.bits, value(2)) }

      println("Log2 magnitude:")
      poke(dut.io.sel, 3)
      step(latency + 1)
      val outTmp = peek(dut.io.out.bits)
      //require( outTmp/divLog )
      //fixTolLSBs.withValue(tolLSBs) { expect(dut.io.out.bits, value(3) * mulLog) }
      step(1)
    }
    poke(dut.io.in.valid, 0)
    poke(dut.io.out.ready, 0)
  }
  def testStream(inp: Seq[Complex], sel: Int, out: Seq[Complex], tolLSBs: Int = 3) {
    require (dut.params.useLast, "This test must have included lastIn and lastOut signal")
    
    val genIn = dut.params.proto.real
    val genLog = dut.params.protoLog
    
    val dataWidth = genIn.getWidth
    val bposIn = (genIn match {
      case fp: FixedPoint => fp.binaryPoint.get
      case _ => 0
    })
    
    val bposLog = (genLog match {
      case fp: FixedPoint => fp.binaryPoint.get
      case _ => 0
    })
    
    val mulLog = if (bposIn > bposLog) math.pow(2, bposIn - bposLog) else 1
    
//    println("Expected result should be: ")
//    out.map(c => println(dut.toString))
    
    val input = inp.iterator
    var cntValidOut = 0
    var cntValidIn = 0
    
    poke(dut.io.in.valid, 1)
    poke(dut.io.sel, sel)
    
    // fill pipes and then enable output side
    while (input.hasNext && peek(dut.io.in.ready)) {
      if (cntValidIn == (inp.length - 1)) {
        poke(dut.io.in.bits, input.next())
        poke(dut.io.lastIn.get, 1)
      }
      else {
        poke(dut.io.in.bits, input.next())
      }
      cntValidIn += 1
      step(1)
    }
    
    poke(dut.io.lastIn.get, 0)
    step(10)
    poke(dut.io.out.ready, 1)
  
    while (cntValidOut < out.length) {
      if (input.hasNext) {
        if (cntValidIn == (inp.length - 1)) {
          poke(dut.io.in.bits, input.next())
          poke(dut.io.lastIn.get, 1)
        }
        else {
          poke(dut.io.in.bits, input.next())
        }
        cntValidIn += 1
      }
      else {
        poke(dut.io.in.valid, 0)
        poke(dut.io.lastIn.get, 0)
      }
      if (peek(dut.io.out.valid)) {
        if (cntValidOut == (out.length - 1)) {
          if (sel == 3)
            fixTolLSBs.withValue(tolLSBs) { expect(dut.io.out.bits, out(cntValidOut)) }
          else
      // if log2 binaryPoint is not equal as binary point of the input data, some kind of division is here necessary
            fixTolLSBs.withValue(tolLSBs) { expect(dut.io.out.bits, out(cntValidOut)) } 
          expect(dut.io.lastOut.get, 1)
        }
        else {
          if (sel == 3)
            fixTolLSBs.withValue(tolLSBs) { expect(dut.io.out.bits, out(cntValidOut)) }
          else
            fixTolLSBs.withValue(tolLSBs) { expect(dut.io.out.bits, out(cntValidOut)) }
          expect(dut.io.lastOut.get, 0)
        }
        cntValidOut += 1
      }
      step(1)
    }
    cntValidOut = 0

    poke(dut.io.in.valid, 0)
    poke(dut.io.out.ready, 0)
    step(2)
    poke(dut.io.out.ready, 1)
    
    // at the same time stream in/ stream out and testing stream with gaps
    for (i <- 0 until inp.size) {
      poke(dut.io.in.valid, 0)
      val delay = 2 //Random.nextInt(5)
      for(i <- 0 to delay) {
        if (peek(dut.io.out.valid) == true) {
          fixTolLSBs.withValue(tolLSBs) { expect(dut.io.out.bits, out(cntValidOut)) }
          cntValidOut += 1
        }
        step(1)
      }
      poke(dut.io.in.valid, 1)
      poke(dut.io.in.bits, inp(i))
      
      if (i == (inp.size - 1))
        poke(dut.io.lastIn.get, 1)
      if (peek(dut.io.out.valid) == true) {
        fixTolLSBs.withValue(tolLSBs) { expect(dut.io.out.bits, out(cntValidOut)) }
        cntValidOut += 1
      }
      step(1)
    }
    poke(dut.io.lastIn.get, 0)
    poke(dut.io.in.valid, 0)
    poke(dut.io.out.ready, 0)
    step(10)
    poke(dut.io.out.ready, 1)
    
    while (cntValidOut < inp.size) {
      if (cntValidOut == inp.size - 1)
        expect(dut.io.lastOut.get, 1)
      if (peek(dut.io.out.valid) == true && peek(dut.io.out.ready)) {
        fixTolLSBs.withValue(tolLSBs) { expect(dut.io.out.bits, out(cntValidOut)) }
        cntValidOut += 1
      }
      step(1)
    }
    step(5)
    cntValidOut = 0
  }

//   def testAXIReadyValidSignals(inp: Seq[Complex], sel: Int, out: Seq[Complex], tolLSBs: Int = 2) {
//     require (dut.params.useLast, "This test must have included lastIn and lastOut signal")
//     
//     println("Expected result should be: ")
//     out.map(c => println(dut.toString))
//     
//     val input = inp.iterator
//     var cntValidOut = 0
//     var cntValidIn = 0
//     
//     poke(dut.io.in.valid, 1)
//     //poke(dut.io.out.ready, 1)
//     poke(dut.io.sel, sel)
//     
//     // define input data untill readyIn is deasserted (initialInDone)
//     while (input.hasNext && peek(dut.io.in.ready)) {
//      poke(dut.io.in.bits, input.next())
//      step()
//     }
//     // wait some time
//     step(10)
//     poke(dut.io.out.ready)
//     
//     while (cntValidOut < out.length) {
//       if (input.hasNext) {
//         if (cntValidIn == (inp.length - 1)) {
//           poke(dut.io.in.bits, input.next())
//           poke(dut.io.lastIn.get, 1)
//         }
//         else {
//           poke(dut.io.in.bits, input.next())
//         }
//         cntValidIn += 1
//       }
//       else {
//         poke(dut.io.in.valid, 0)
//         poke(dut.io.lastIn.get, 0)
//       }
//       if (peek(dut.io.out.valid)) {
//         if (cntValidOut == (out.length - 1)) {
//           fixTolLSBs.withValue(tolLSBs) { expect(dut.io.out.bits, out(cntValidOut)) }
//           expect(dut.io.lastOut.get, 1)
//         }
//         else {
//           fixTolLSBs.withValue(tolLSBs) { expect(dut.io.out.bits, out(cntValidOut)) }
//           expect(dut.io.lastOut.get, 0)
//         }
//         cntValidOut += 1 // here check lastOut
//       }
//       step(1)
//     }
//   }
}



object FixedMagTester {
  def apply(params: MAGParams[FixedPoint], inp: Seq[Complex], out: Seq[Seq[Complex]]): Boolean = {
		chisel3.iotesters.Driver.execute(Array("-tbn", "verilator"),
      () => new LogMagMux(params)) { c => 
      new LogMagMuxTester(c) {
        testMux(inp, out)
      }}
  }
  // for stream tester
  def apply(params: MAGParams[FixedPoint], inp: Seq[Complex], out: Seq[Complex], sel: Int) = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator"),
      () => new LogMagMux(params)) { c => 
      new LogMagMuxTester(c) {
        testStream(inp, sel, out)
    }}
  }
}

