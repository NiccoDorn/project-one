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

        // 11110000 00000000 00000000 00000000
        c.io.input.poke("hF0000000".U)
        c.io.result.expect(0.U)
        
        // 00000000 10000000 00000000 00000000
        c.io.input.poke("h00800000".U)
        c.io.result.expect(8.U)
        
        // 00000000 00000000 01110000 00000000
        c.io.input.poke("h00007000".U)
        c.io.result.expect(17.U)
        
        // 00000000 00000000 00000000 00000001
        c.io.input.poke("h00000001".U)
        c.io.result.expect(31.U)
    }
  }


}
