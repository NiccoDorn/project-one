package bitmanipulation

import chisel3._
import chisel3.util._
import cats.syntax.validated

abstract class AbstractGeneralizedReverser(bitWidth: Int) extends Module {
  val io = IO(new Bundle {
    val input = Input(UInt(bitWidth.W))
    val pattern = Input(UInt(log2Ceil(bitWidth).W))
    val result = Output(UInt(bitWidth.W))
  })
}

class GeneralizedReverser(bitWidth: Int)
    extends AbstractGeneralizedReverser(bitWidth) {

      // Simple assertion
      assert(isPow2(bitWidth), s"Bit width must be power of two")

      if (bitWidth == 1) {
        io.result := io.input
      } 
      else {
        val mid = bitWidth / 2
        val lower = io.input(mid - 1, 0)
        val upper = io.input(bitWidth - 1, mid)
        val currentPhase = log2Ceil(bitWidth)

        // Lower submodule
        val lowerReversed = Module(new GeneralizedReverser(mid))
        lowerReversed.io.input := lower
        lowerReversed.io.pattern := io.pattern
        
        // Upper submodule
        val upperReversed = Module(new GeneralizedReverser(mid))
        upperReversed.io.input := upper
        upperReversed.io.pattern := io.pattern

        val swap = io.pattern(currentPhase - 1)

        val outLower = Mux(swap, upperReversed.io.result, lowerReversed.io.result)
        val outUpper = Mux(swap, lowerReversed.io.result, upperReversed.io.result)

        io.result := Cat(outUpper, outLower)

  }
}
