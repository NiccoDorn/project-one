package project1.public.RISCV.RV32B

import RISCV.implementation.Core
import RISCV.implementation.RV32B._
import RISCV.utils.assembler.RISCVAssembler
import bitmanipulation.FixedRotater
import bitmanipulation.GeneralizedReverser
import bitmanipulation.SequentialRotater
import bitmanipulation.Shuffler
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import project1.utils.LUI_ADDI_Unit
import project1.utils.ProcessorTestUtils._
import project1.utils.RISCVInstruction._
import project1.utils.models._

class BitPermutationTest
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers {

  behavior of "BitPermutationUnit"

  it should "do grev with 0xAAAAAAAA and 0b00001" in {
    test(
      new Core(
        Seq(
          () => new LUI_ADDI_Unit,
          () =>
            new BitPermutationUnit(
              () => new GeneralizedReverser(32),
              () => new Shuffler(32),
              () => new SequentialRotater(32, () => new FixedRotater(32, 1))
            )
        )
      )
    ).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      var registers =
        generateRandomMap(signedToUnsigned(getSignedValue(0xffffffff, 32), 32))
      registers = registers + (2 -> 0xaaaaaaaa)
      registers = registers + (3 -> 0b00001)
      resetCore(dut)
      val state = prepareState(dut, registers)
      val instr = BigInt(
        RISCVAssembler.fromString("grev x1, x2, x3").replace("\n", ""),
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
        rd_wdata = signedToUnsigned(0x55555555, 32).U
        pc_rdata = state.pc.U
        pc_wdata = (state.pc + 4).U

      }
      evaluateRVFI(dut, expected)
    }
  }

  it should "do shfl with 0xAAAAAAAA and 0b00001" in {
    test(
      new Core(
        Seq(
          () => new LUI_ADDI_Unit,
          () =>
            new BitPermutationUnit(
              () => new GeneralizedReverser(32),
              () => new Shuffler(32),
              () => new SequentialRotater(32, () => new FixedRotater(32, 1))
            )
        )
      )
    ).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      var registers =
        generateRandomMap(signedToUnsigned(getSignedValue(0xffffffff, 32), 32))
      registers = registers + (2 -> 0xaaaaaaaa)
      registers = registers + (3 -> 0b00001)
      resetCore(dut)
      val state = prepareState(dut, registers)
      val instr = BigInt(
        RISCVAssembler.fromString("shfl x1, x2, x3").replace("\n", ""),
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
        rd_wdata = signedToUnsigned(0xcccccccc, 32).U
        pc_rdata = state.pc.U
        pc_wdata = (state.pc + 4).U

      }
      evaluateRVFI(dut, expected)
    }
  }

  it should "do rotate right with 0xAAAAAAAA and 0b00001" in {
    test(
      new Core(
        Seq(
          () => new LUI_ADDI_Unit,
          () =>
            new BitPermutationUnit(
              () => new GeneralizedReverser(32),
              () => new Shuffler(32),
              () => new SequentialRotater(32, () => new FixedRotater(32, 1))
            )
        )
      )
    ).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      var registers =
        generateRandomMap(
          signedToUnsigned(getSignedValue(0xffffffff, 32), 32)
        )
      registers = registers + (2 -> 0xaaaaaaaa)
      registers = registers + (3 -> 0b00001)
      resetCore(dut)
      val state = prepareState(dut, registers)
      val instr = BigInt(
        RISCVAssembler.fromString("ror x1, x2, x3").replace("\n", ""),
        16
      ).U
      executeInstruction(dut, instr)
      dut.clock.step(1)
      val expected = new RVFI {
        valid = true.B
        order = state.retire_count.U
        insn = instr
        rs1_addr = 2.U
        rs2_addr = 3.U
        rd_addr = 1.U
        rs1_rdata = signedToUnsigned(registers(2), 32).U
        rs2_rdata = signedToUnsigned(registers(3), 32).U
        rd_wdata = signedToUnsigned(0x55555555, 32).U
        pc_rdata = state.pc.U
        pc_wdata = (state.pc + 4).U

      }
      evaluateRVFI(dut, expected)
    }
  }

  it should "do rotate right with 0xAAAAAAAA and immediate 0b00001" in {
    test(
      new Core(
        Seq(
          () => new LUI_ADDI_Unit,
          () =>
            new BitPermutationUnit(
              () => new GeneralizedReverser(32),
              () => new Shuffler(32),
              () => new SequentialRotater(32, () => new FixedRotater(32, 1))
            )
        )
      )
    ).withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) {
      dut =>
        var registers =
          generateRandomMap(
            signedToUnsigned(getSignedValue(0xffffffff, 32), 32)
          )
        registers = registers + (2 -> 0xaaaaaaaa)
        resetCore(dut)
        val state = prepareState(dut, registers)
        val instr = BigInt(
          RISCVAssembler
            .fromString(s"rori x1, x2, ${signedToUnsigned(0b00001, 32)}")
            .replace("\n", ""),
          16
        ).U
        executeInstruction(dut, instr)
        dut.clock.step(1)
        val expected = new RVFI {
          valid = true.B
          order = state.retire_count.U
          insn = instr
          rs1_addr = 2.U
          rs2_addr = 0.U
          rd_addr = 1.U
          rs1_rdata = signedToUnsigned(registers(2), 32).U
          rs2_rdata = 0.U
          rd_wdata = signedToUnsigned(0x55555555, 32).U
          pc_rdata = state.pc.U
          pc_wdata = (state.pc + 4).U

        }
        evaluateRVFI(dut, expected)
    }
  }

  it should "do rotate left with 0xAAAAAAAA and 0b00001" in {
    test(
      new Core(
        Seq(
          () => new LUI_ADDI_Unit,
          () =>
            new BitPermutationUnit(
              () => new GeneralizedReverser(32),
              () => new Shuffler(32),
              () => new SequentialRotater(32, () => new FixedRotater(32, 1))
            )
        )
      )
    ).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      var registers =
        generateRandomMap(
          signedToUnsigned(getSignedValue(0xffffffff, 32), 32)
        )
      registers = registers + (2 -> 0xaaaaaaaa)
      registers = registers + (3 -> 0b00001)
      resetCore(dut)
      val state = prepareState(dut, registers)
      val instr = BigInt(
        RISCVAssembler.fromString("rol x1, x2, x3").replace("\n", ""),
        16
      ).U
      executeInstruction(dut, instr)
      dut.clock.step(1)
      val expected = new RVFI {
        valid = true.B
        order = state.retire_count.U
        insn = instr
        rs1_addr = 2.U
        rs2_addr = 3.U
        rd_addr = 1.U
        rs1_rdata = signedToUnsigned(registers(2), 32).U
        rs2_rdata = signedToUnsigned(registers(3), 32).U
        rd_wdata = signedToUnsigned(0x55555555, 32).U
        pc_rdata = state.pc.U
        pc_wdata = (state.pc + 4).U

      }
      evaluateRVFI(dut, expected)
    }
  }

  it should "assemble a perm pseudo instruction" in {
    val instr =
      "perm x1, x2, 1, 0, 3, 2, 5, 4, 7, 6, 9, 8, 11, 10, 13, 12, 15, 14, 17, 16, 19, 18, 21, 20, 23, 22, 25, 24, 27, 26, 29, 28, 31, 30"
    RISCVAssembler.fromString(instr)
  }

}
