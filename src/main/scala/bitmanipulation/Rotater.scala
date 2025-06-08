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
  Rotater.io <> DontCare
  
  // idea: we use the above defined fixed rotator {Rotator} for the sequential rotator
  // for a shift amount of times, making this algorithm linear time
  // like this we can simply use the 1-bit rotator, apply it n times
  // and count {cnt} how often we used it.

  val rotate = RegInit(false.B)
  val cnt = RegInit(0.U(log2Ceil(bitWidth).W))
  val shamtT = RegInit(0.U(log2Ceil(bitWidth).W))
  val currVal = RegInit(0.U(bitWidth.W))
  
  io.done := false.B
  io.result := currVal
  
  Rotater.io.input := currVal
  
  when(!rotate) {
    when(io.start) {
      when(io.shamt === 0.U) {
        currVal := io.input
        io.done := true.B
      }.otherwise {
        currVal := io.input
        shamtT := io.shamt
        cnt := 0.U
        rotate := true.B
      }
    }
  }.otherwise {
    currVal := Rotater.io.result
    cnt := cnt + 1.U
    
    when(cnt + 1.U === shamtT) { // done, rotated cnt = 0 ... n-1 = n times
      rotate := false.B
      io.done := true.B
    }
  }
}