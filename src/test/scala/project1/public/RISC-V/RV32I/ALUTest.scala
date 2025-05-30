package project1.public.RISCV.RV32I

import RISCV.implementation.RV32I.ALU
import RISCV.model.ALU_CONTROL
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ALUTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "ALU"

  it should "perform addition correctly" in {
    test(new ALU) { dut =>
      dut.io_alu.alu_op.poke(ALU_CONTROL.ADD)
      dut.io_alu.op1.poke(5.U)
      dut.io_alu.op2.poke(3.U)
      dut.clock.step()

      dut.io_alu.result.expect(8.U)
    }
  }

  it should "perform subtraction correctly" in {
    test(new ALU) { dut =>
      dut.io_alu.alu_op.poke(ALU_CONTROL.SUB)
      dut.io_alu.op1.poke(5.U)
      dut.io_alu.op2.poke(3.U)
      dut.clock.step()

      dut.io_alu.result.expect(2.U)
    }
  }

  it should "perform bitwise AND correctly" in {
    test(new ALU) { dut =>
      dut.io_alu.alu_op.poke(ALU_CONTROL.AND)
      dut.io_alu.op1.poke(5.U)
      dut.io_alu.op2.poke(3.U)
      dut.clock.step()

      dut.io_alu.result.expect(1.U)
    }
  }

  it should "perform bitwise OR correctly" in {
    test(new ALU) { dut =>
      dut.io_alu.alu_op.poke(ALU_CONTROL.OR)
      dut.io_alu.op1.poke(5.U)
      dut.io_alu.op2.poke(3.U)
      dut.clock.step()

      dut.io_alu.result.expect(7.U)
    }
  }

  it should "perform unsigned less than correctly" in {
    test(new ALU) { dut =>
      dut.io_alu.alu_op.poke(ALU_CONTROL.SLTU)
      dut.io_alu.op1.poke(5.U)
      dut.io_alu.op2.poke(3.U)
      dut.clock.step()

      dut.io_alu.result.expect(0.U)
    }
  }

  it should "return zero for unknown ALU control code" in {
    test(new ALU) { dut =>
      dut.io_alu.alu_op.poke(ALU_CONTROL.UNKNOWN)
      dut.io_alu.op1.poke(5.U)
      dut.io_alu.op2.poke(3.U)
      dut.clock.step()

      dut.io_alu.result.expect(0.U)
    }
  }
}
