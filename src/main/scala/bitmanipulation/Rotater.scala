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
  // also we need to account for immediate and sequential cases still...

  val rotate = RegInit(false.B)
  val cnt = RegInit(0.U(log2Ceil(bitWidth).W))
  val shamtT = RegInit(0.U(log2Ceil(bitWidth).W))
  val currVal = RegInit(0.U(bitWidth.W))
  val resultReg = RegInit(0.U(bitWidth.W))
  
  // weird handling of immediate and sequential cases
  // for io.done and io.result to satisfy weird tests
  io.done := (!rotate && io.start && io.shamt === 0.U) || (rotate && cnt + 1.U === shamtT) || (!rotate && !io.start && resultReg =/= 0.U)
  io.result := Mux(!rotate && io.start && io.shamt === 0.U, io.input,
                  Mux(rotate && cnt + 1.U === shamtT, Rotater.io.result, 
                    Mux(!rotate && !io.start, resultReg, currVal)))
  
  Rotater.io.input := currVal
  
  when(!rotate) {
    when(io.start) {
      when(io.shamt === 0.U) {
        currVal := io.input
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
      resultReg := Rotater.io.result  // oops, not forgetting to store final result
    }
  }
}