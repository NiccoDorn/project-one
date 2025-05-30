package project1.public.RISCV.RV32B

import RISCV.implementation.Core
import RISCV.implementation.RV32B._
import RISCV.utils.assembler.RISCVAssembler
import bitmanipulation.LeadingZerosCounter
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import project1.utils.LUI_ADDI_Unit
import project1.utils.ProcessorTestUtils._
import project1.utils.RISCVInstruction._
import project1.utils.models._

class BasicBitManipulationTest
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers {

  behavior of "BasicBitManipulationUnit"

  it should "count the leading zeros" in {
    test(
      new Core(
        Seq(
          () => new LUI_ADDI_Unit,
          () => new BasicBitManipulationUnit(() => new LeadingZerosCounter(32))
        )
      )
    ).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      var registers =
        generateRandomMap(signedToUnsigned(getSignedValue(0xffffffff, 32), 32))
      registers = registers + (3 -> 0x0000ffff)
      resetCore(dut)
      val state = prepareState(dut, registers)
      val instr =
        BigInt(RISCVAssembler.fromString("clz x1, x3").replace("\n", ""), 16).U
      executeInstruction(dut, instr)
      val expected = new RVFI {
        valid = true.B
        order = state.retire_count.U
        insn = instr
        rs1_addr = 3.U
        rs2_addr = 0.U
        rd_addr = 1.U
        rs1_rdata = registers(3).U
        rs2_rdata = 0.U
        rd_wdata = 0x00000010.U
        pc_rdata = state.pc.U
        pc_wdata = (state.pc + 4).U

      }
      evaluateRVFI(dut, expected)
    }
  }

  it should "count the trailing zeros" in {
    test(
      new Core(
        Seq(
          () => new LUI_ADDI_Unit,
          () => new BasicBitManipulationUnit(() => new LeadingZerosCounter(32))
        )
      )
    ).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      var registers =
        generateRandomMap(signedToUnsigned(getSignedValue(0xffffffff, 32), 32))
      registers = registers + (3 -> 0x0000ff00)
      resetCore(dut)
      val state = prepareState(dut, registers)
      val instr =
        BigInt(RISCVAssembler.fromString("ctz x1, x3").replace("\n", ""), 16).U
      executeInstruction(dut, instr)
      val expected = new RVFI {
        valid = true.B
        order = state.retire_count.U
        insn = instr
        rs1_addr = 3.U
        rs2_addr = 0.U
        rd_addr = 1.U
        rs1_rdata = registers(3).U
        rs2_rdata = 0.U
        rd_wdata = 0x00000008.U
        pc_rdata = state.pc.U
        pc_wdata = (state.pc + 4).U

      }
      evaluateRVFI(dut, expected)
    }
  }

  it should "do min" in {
    test(
      new Core(
        Seq(
          () => new LUI_ADDI_Unit,
          () => new BasicBitManipulationUnit(() => new LeadingZerosCounter(32))
        )
      )
    ).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      var registers =
        generateRandomMap(signedToUnsigned(getSignedValue(0xffffffff, 32), 32))
      registers = registers + (2 -> 0x00000f00)
      registers = registers + (3 -> 0x0000ff00)
      resetCore(dut)
      val state = prepareState(dut, registers)
      val instr = BigInt(
        RISCVAssembler.fromString("min x1, x2, x3").replace("\n", ""),
        16
      ).U
      executeInstruction(dut, instr)
      val expected = new RVFI {
        valid = true.B
        order = state.retire_count.U
        insn = instr
        rs1_addr = 2.U
        rs2_addr = 3.U
        rd_addr = 1.U
        rs1_rdata = registers(2).U
        rs2_rdata = registers(3).U
        rd_wdata = registers(2).U
        pc_rdata = state.pc.U
        pc_wdata = (state.pc + 4).U

      }
      evaluateRVFI(dut, expected)
    }
  }

  it should "do min with signed numbers" in {
    test(
      new Core(
        Seq(
          () => new LUI_ADDI_Unit,
          () => new BasicBitManipulationUnit(() => new LeadingZerosCounter(32))
        )
      )
    ).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      var registers =
        generateRandomMap(signedToUnsigned(getSignedValue(0xffffffff, 32), 32))
      registers = registers + (2 -> 0x00000f00)
      registers = registers + (3 -> 0xf000ff00)
      resetCore(dut)
      val state = prepareState(dut, registers)
      val instr = BigInt(
        RISCVAssembler.fromString("min x1, x2, x3").replace("\n", ""),
        16
      ).U
      executeInstruction(dut, instr)
      val expected = new RVFI {
        valid = true.B
        order = state.retire_count.U
        insn = instr
        rs1_addr = 2.U
        rs2_addr = 3.U
        rd_addr = 1.U
        rs1_rdata = signedToUnsigned(registers(2), 32).U
        rs2_rdata = signedToUnsigned(registers(3), 32).U
        rd_wdata = signedToUnsigned(registers(3), 32).U
        pc_rdata = state.pc.U
        pc_wdata = (state.pc + 4).U

      }
      evaluateRVFI(dut, expected)
    }
  }

  it should "do minu with unsigned numbers" in {
    test(
      new Core(
        Seq(
          () => new LUI_ADDI_Unit,
          () => new BasicBitManipulationUnit(() => new LeadingZerosCounter(32))
        )
      )
    ).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      var registers =
        generateRandomMap(signedToUnsigned(getSignedValue(0xffffffff, 32), 32))
      registers = registers + (2 -> 0x00000f00)
      registers = registers + (3 -> 0xf000ff00)
      resetCore(dut)
      val state = prepareState(dut, registers)
      val instr = BigInt(
        RISCVAssembler.fromString("minu x1, x2, x3").replace("\n", ""),
        16
      ).U
      executeInstruction(dut, instr)
      val expected = new RVFI {
        valid = true.B
        order = state.retire_count.U
        insn = instr
        rs1_addr = 2.U
        rs2_addr = 3.U
        rd_addr = 1.U
        rs1_rdata = signedToUnsigned(registers(2), 32).U
        rs2_rdata = signedToUnsigned(registers(3), 32).U
        rd_wdata = signedToUnsigned(registers(2), 32).U
        pc_rdata = state.pc.U
        pc_wdata = (state.pc + 4).U

      }
      evaluateRVFI(dut, expected)
    }
  }

  it should "do max with signed numbers" in {
    test(
      new Core(
        Seq(
          () => new LUI_ADDI_Unit,
          () => new BasicBitManipulationUnit(() => new LeadingZerosCounter(32))
        )
      )
    ).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      var registers =
        generateRandomMap(signedToUnsigned(getSignedValue(0xffffffff, 32), 32))
      registers = registers + (2 -> 0x00000f00)
      registers = registers + (3 -> 0xf000ff00)
      resetCore(dut)
      val state = prepareState(dut, registers)
      val instr = BigInt(
        RISCVAssembler.fromString("max x1, x2, x3").replace("\n", ""),
        16
      ).U
      executeInstruction(dut, instr)
      val expected = new RVFI {
        valid = true.B
        order = state.retire_count.U
        insn = instr
        rs1_addr = 2.U
        rs2_addr = 3.U
        rd_addr = 1.U
        rs1_rdata = signedToUnsigned(registers(2), 32).U
        rs2_rdata = signedToUnsigned(registers(3), 32).U
        rd_wdata = signedToUnsigned(registers(2), 32).U
        pc_rdata = state.pc.U
        pc_wdata = (state.pc + 4).U

      }
      evaluateRVFI(dut, expected)
    }
  }

  it should "do maxu with unsigned numbers" in {
    test(
      new Core(
        Seq(
          () => new LUI_ADDI_Unit,
          () => new BasicBitManipulationUnit(() => new LeadingZerosCounter(32))
        )
      )
    ).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      var registers =
        generateRandomMap(signedToUnsigned(getSignedValue(0xffffffff, 32), 32))
      registers = registers + (2 -> 0x00000f00)
      registers = registers + (3 -> 0xf000ff00)
      resetCore(dut)
      val state = prepareState(dut, registers)
      val instr = BigInt(
        RISCVAssembler.fromString("maxu x1, x2, x3").replace("\n", ""),
        16
      ).U
      executeInstruction(dut, instr)
      val expected = new RVFI {
        valid = true.B
        order = state.retire_count.U
        insn = instr
        rs1_addr = 2.U
        rs2_addr = 3.U
        rd_addr = 1.U
        rs1_rdata = signedToUnsigned(registers(2), 32).U
        rs2_rdata = signedToUnsigned(registers(3), 32).U
        rd_wdata = signedToUnsigned(registers(3), 32).U
        pc_rdata = state.pc.U
        pc_wdata = (state.pc + 4).U

      }
      evaluateRVFI(dut, expected)
    }
  }

  it should "count the population" in {
    test(
      new Core(
        Seq(
          () => new LUI_ADDI_Unit,
          () => new BasicBitManipulationUnit(() => new LeadingZerosCounter(32))
        )
      )
    ).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      var registers =
        generateRandomMap(signedToUnsigned(getSignedValue(0xffffffff, 32), 32))
      registers = registers + (2 -> 0x00000f00)
      resetCore(dut)
      val state = prepareState(dut, registers)
      val instr =
        BigInt(RISCVAssembler.fromString("cpop x1, x2").replace("\n", ""), 16).U
      executeInstruction(dut, instr)
      val expected = new RVFI {
        valid = true.B
        order = state.retire_count.U
        insn = instr
        rs1_addr = 2.U
        rs2_addr = 0.U
        rd_addr = 1.U
        rs1_rdata = signedToUnsigned(registers(2), 32).U
        rs2_rdata = 0.U
        rd_wdata = signedToUnsigned(0x4, 32).U
        pc_rdata = state.pc.U
        pc_wdata = (state.pc + 4).U

      }
      evaluateRVFI(dut, expected)
    }
  }

}
