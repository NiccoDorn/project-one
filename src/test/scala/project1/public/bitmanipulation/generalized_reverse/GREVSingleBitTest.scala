package project1.public.bitmanipulation.generalized_reverse

import bitmanipulation.GeneralizedReverser
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GREVSingleBitTest
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers {
  behavior of "GREVSingleBit"

  "GeneralizedReverse" should "do nothing" in {
    test(new GeneralizedReverser(32)).withAnnotations(Seq(WriteVcdAnnotation)) {
      c =>
        val input = BigInt("AAAAAAAA", 16).U(32.W)
        val pattern = 0.U(5.W)
        val expected = input

        c.io.input.poke(input)
        c.io.pattern.poke(pattern)
        c.clock.step()
        c.io.result.expect(expected)
    }
  }

}
