package bitmanipulation

import chisel3._
import chisel3.util._

abstract class AbstractFixedRotater(bitWidth: Int) extends Module {
  val io = IO(new Bundle {
    val input = Input(UInt(bitWidth.W))
    val result = Output(UInt(bitWidth.W))
  })
}

class FixedRotater(bitWidth: Int, shamt: Int)
    extends AbstractFixedRotater(bitWidth) {
      
      // Simple assertion
      assert(isPow2(bitWidth), s"Bit width must be power of two")
      assert(shamt >= 0)

      // Simple bit-manipulation with right and left shift and then OR two results
      val rightShift = io.input >> shamt
      val leftShift  = io.input << (bitWidth - shamt)
      io.result := (rightShift | leftShift)

      

}

abstract class AbstractSequentialRotater(bitWidth: Int) extends Module {
  val io = IO(new Bundle {
    val input = Input(UInt(bitWidth.W))
    val shamt = Input(UInt(log2Ceil(bitWidth).W))
    val start = Input(Bool())
    val done = Output(Bool())
    val result = Output(UInt(bitWidth.W))
  })
}

class SequentialRotater(bitWidth: Int, generator: () => AbstractFixedRotater)
    extends AbstractSequentialRotater(bitWidth) {
  val Rotater = Module(generator())
  // idea: we use the above defined fixed rotator {Rotator} for the sequential rotator
  // for a shift amount of times, making this algorithm linear time
  // like this we can simply use the 1-bit rotator, apply it n times
  // and count {cnt} how often we used it.
  // We use a state machine approach to handle immediate, idle and rotation cases
  // in the sequential rotater
  
  val sIdle :: sRotate :: sDone :: Nil = Enum(3) // good old ocaml list states hehe
  val state = RegInit(sIdle)
  
  val cnt = RegInit(0.U(log2Ceil(bitWidth).W))
  val shamtT = RegInit(0.U(log2Ceil(bitWidth).W))
  val currVal = RegInit(0.U(bitWidth.W))
  val resultReg = RegInit(0.U(bitWidth.W))
  
  val doneTemp = WireDefault(false.B)
  val resultTemp = WireDefault(0.U(bitWidth.W))
  
  // now
  switch(state) {
    is(sIdle) {
      doneTemp := io.start && io.shamt === 0.U  // immediate
      resultTemp := Mux(io.start && io.shamt === 0.U, io.input, 0.U)
    }
    is(sRotate) {
      doneTemp := cnt + 1.U === shamtT  // just finished, provide done and result
      resultTemp := Mux(cnt + 1.U === shamtT, Rotater.io.result, currVal)
    }
    is(sDone) {
      doneTemp := true.B
      resultTemp := resultReg
    }
  }
  io.done := doneTemp
  io.result := resultTemp
  
  // next - actual rotation
  Rotater.io.input := currVal
  switch(state) {
    is(sIdle) {
      when(io.start) {
        when(io.shamt === 0.U) {
          // no shifting needed, immediately assign currVal the provided input
          currVal := io.input
        }.otherwise {
          // rotation case with shamt > 0
          currVal := io.input
          shamtT := io.shamt
          cnt := 0.U
          state := sRotate
        }
      }
    }
    
    is(sRotate) {
      currVal := Rotater.io.result // only now pass to fixed rotater
      cnt := cnt + 1.U
      when(cnt + 1.U === shamtT) { // check if already finished
        resultReg := Rotater.io.result // overwrite io.result as soon as possible
        state := sDone  // same goes for state
      }
    }
    
    is(sDone) {
      when(!io.start) {
        state := sIdle
      }
    }
  }
}