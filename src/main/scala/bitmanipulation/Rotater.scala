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
      assert(shamt >= 0 && shamt < bitWidth)

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

  ??? // TODO: implement Task 1.4 here

}
