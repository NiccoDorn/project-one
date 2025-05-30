package bitmanipulation

import chisel3._
import chisel3.util._

abstract class AbstractShuffler(bitWidth: Int) extends Module {
  val io = IO(new Bundle {
    val input = Input(UInt(bitWidth.W))
    val pattern = Input(UInt((log2Ceil(bitWidth) - 1).W))
    val unshuffle = Input(UInt(1.W))
    val result = Output(UInt(bitWidth.W))
  })
}

class Shuffler(bitWidth: Int) extends AbstractShuffler(bitWidth) {

  ??? // TODO: implement Task 1.3 here

}
