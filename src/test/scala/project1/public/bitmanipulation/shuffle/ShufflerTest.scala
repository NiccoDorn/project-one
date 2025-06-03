package project1.public.bitmanipulation.shuffle

import bitmanipulation.Shuffler
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ShufflerTest
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers {
  behavior of "Shuffler"

  it should "do nothing" in {
    test(new Shuffler(32)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val input = BigInt("AAAAAAAA", 16).U(32.W)
      val unshuffle = 0.U(1.W)
      val pattern = 0.U(5.W)
      val expected = input

      c.io.input.poke(input)
      c.io.pattern.poke(pattern)
      c.io.unshuffle.poke(unshuffle)
      c.io.result.expect(expected)
    }
  }

  it should "do shuffle" in {
    test(new Shuffler(32)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val input = BigInt("0000FFFF", 16).U(32.W)
      val unshuffle = 0.U(1.W)
      val pattern = 0b1111.U(4.W)
      val expected = BigInt("55555555", 16).U(32.W)

      c.io.input.poke(input)
      c.io.pattern.poke(pattern)
      c.io.unshuffle.poke(unshuffle)
      c.io.result.expect(expected)
    }
  }

  it should "do unshuffle" in {
    test(new Shuffler(32)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val input = BigInt("AAAAAAAA", 16).U(32.W)
      val unshuffle = 1.U(1.W)
      val pattern = 0b1111.U(4.W)
      val expected = BigInt("FFFF0000", 16).U(32.W)

      c.io.input.poke(input)
      c.io.pattern.poke(pattern)
      c.io.unshuffle.poke(unshuffle)
      c.io.result.expect(expected)
    }
  }
}
