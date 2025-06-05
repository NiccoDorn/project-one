package bitmanipulation

import chisel3._
import chisel3.util._

abstract class AbstractGeneralizedReverser(bitWidth: Int) extends Module {
  val io = IO(new Bundle {
    val input = Input(UInt(bitWidth.W))
    val pattern = Input(UInt(log2Ceil(bitWidth).W))
    val result = Output(UInt(bitWidth.W))
  })
}

class GeneralizedReverser(bitWidth: Int)
    extends AbstractGeneralizedReverser(bitWidth) {

  if (bitWidth == 2) {

    let swap = io.pattern > 0.U && io.pattern < 3.U
    let i1 = io.input(0)
    let i2 = io.input(1)

    io.result = Mux(swap, Cat(i1, i2), io.input)

  } else {

    val mid = bitWidth / 2
    val lower = io.input(mid - 1, 0)
    val upper = io.input(bitWidth - 1, mid)

    val lowerReversed = Module(new GeneralizedReverser(mid))
    val upperReversed = Module(new GeneralizedReverser(mid))

    lowerReversed.io.input := lower
    lowerReversed.io.pattern := io.pattern

    upperReversed.io.input = upper
    upperReversed.io.pattern = io.pattern

    val swap = io.pattern >= log2Ceil(bitWidth).U

    val outLower = Mux(swap, upperReversed.io.result, lowerReversed.io.result)
    val outUpper = Mux(swap, lowerReversed.io.result, upperReversed.io.result)

    io.result := Cat(outUpper, outLower)

  }
}
