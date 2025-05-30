package project1.public.bitmanipulation.clz

import bitmanipulation.LeadingZerosCounter
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LeadingZerosCounterTest
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers {
  behavior of "LeadingZerosCounter"

  "LeadingZerosCounter(32)" should "count number of leading bits correctly" in {
    test(new LeadingZerosCounter(32)).withAnnotations(Seq(WriteVcdAnnotation)) {
      c =>
        c.io.input.poke(0.U)
        c.io.result.expect(32.U)
    }
  }

}
