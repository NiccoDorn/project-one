package project1.public.bitmanipulation.rotate

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import bitmanipulation._

class RotateBitTest
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers {
  behavior of "Rotater"

  "SequentialRotater" should "rotate right by 0" in {
    test(new SequentialRotater(32, () => new FixedRotater(32, 1)))
      .withAnnotations(Seq(WriteVcdAnnotation)) { c =>
        c.io.input.poke(0.U(32.W))
        c.io.shamt.poke(0.U(5.W))
        c.io.start.poke(true.B)
        c.io.result.expect(0.U(32.W))
        c.io.done.expect(true.B)
      }
  }

  "FixedRotater" should "rotate right by 0" in {
    test(new FixedRotater(32, 0))
      .withAnnotations(Seq(WriteVcdAnnotation)) { c =>
        c.io.input.poke(0.U(32.W))
        c.io.result.expect(0.U(32.W))
      }
  }

}
