package project1.public.RISCV.RV32I

import RISCV.implementation.Core
import RISCV.implementation.RV32I._
import RISCV.model._
import RISCV.utils.assembler.RISCVAssembler
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InitialTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  it should "be a ALU" in {
    test(new ALU) { dut =>
      dut.io_alu.alu_op.poke(ALU_CONTROL.ADD)
      dut.io_alu.op2.poke(3.U)
      dut.io_alu.op1.poke(5.U)
      dut.clock.step()
      dut.io_alu.result.expect(8.U)
    }
  }

  it should "be a Decoder" in {
    test(new Decoder) { dut =>
      dut.io_decoder.instr.poke(0x00000013.U)
      dut.clock.step()
      dut.io_decoder.rs1.expect(0.U)
      dut.io_decoder.rs2.expect(0.U)
      dut.io_decoder.rd.expect(0.U)
      dut.io_decoder.imm.expect(0.U)
    }
  }

  it should "decode a negative value" in {
    test(new Decoder) { dut =>
      val instr = BigInt(
        RISCVAssembler.fromString("bne x17 x4 0x-4").replace("\n", ""),
        16
      ).U
      dut.io_decoder.instr.poke(instr)
      dut.clock.step()
      dut.io_decoder.rs1.expect(17.U)
      dut.io_decoder.rs2.expect(4.U)
      dut.io_decoder.rd.expect(0.U)
      dut.io_decoder.imm.expect(BigInt("FFFFFFFC", 16).U)
    }
  }

  it should "detect a branch" in {
    test(new ControlUnit) { dut =>
      dut.io_ctrl.instr_type.poke(RISCV_TYPE.beq)
      dut.clock.step()
      dut.io_ctrl.next_pc_select.expect(NEXT_PC_SELECT.BRANCH)
    }
  }

  it should "execute a nop" in {
    val nop = RISCVAssembler.fromString("nop").split("\n")(0)
    test(
      new Core(
        Seq(() =>
          new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
        )
      )
    ).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Reset the core
      dut.io_reset.boot_addr.poke(0.U)
      dut.io_reset.rst_n.poke(false.B)
      dut.io_instr.instr_gnt.poke(false.B)
      dut.io_data.data_gnt.poke(false.B)
      dut.clock.step()
      dut.io_reset.rst_n.poke(true.B)

      dut.clock.step()
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(0.U)
      dut.io_instr.instr_rdata.poke(Integer.parseInt(nop, 16).U)
      dut.io_instr.instr_gnt.poke(true.B)

      dut.clock.step()
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(4.U)
      dut.io_instr.instr_rdata.poke(Integer.parseInt(nop, 16).U)
      dut.io_instr.instr_gnt.poke(true.B)

      dut.io_rvfi.rvfi_valid.expect(true.B)
      dut.io_rvfi.rvfi_order.expect(0.U)
      dut.io_rvfi.rvfi_insn.expect(Integer.parseInt(nop, 16).U)
      dut.io_rvfi.rvfi_trap.expect(false.B)
      dut.io_rvfi.rvfi_halt.expect(false.B)
      dut.io_rvfi.rvfi_intr.expect(false.B)
      dut.io_rvfi.rvfi_mode.expect(0.U)
      dut.io_rvfi.rvfi_ixl.expect(0.U)

      dut.io_rvfi.rvfi_rs1_addr.expect(0.U)
      dut.io_rvfi.rvfi_rs2_addr.expect(0.U)
      dut.io_rvfi.rvfi_rs1_rdata.expect(0.U)
      dut.io_rvfi.rvfi_rs2_rdata.expect(0.U)

      dut.io_rvfi.rvfi_rd_addr.expect(0.U)
      dut.io_rvfi.rvfi_rd_wdata.expect(0.U)

      dut.io_rvfi.rvfi_pc_rdata.expect(0.U)
      dut.io_rvfi.rvfi_pc_wdata.expect(4.U)

    }
  }

  it should "execute a few instructions" in {
    val addi = RISCVAssembler.fromString("addi x1, x0, 0x0ab").split("\n")(0)
    val add = RISCVAssembler.fromString("add x2, x1, x1").split("\n")(0)
    test(
      new Core(
        Seq(() =>
          new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
        )
      )
    ).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Reset the core
      dut.io_reset.boot_addr.poke(0.U)
      dut.io_reset.rst_n.poke(false.B)
      dut.io_instr.instr_gnt.poke(false.B)
      dut.io_data.data_gnt.poke(false.B)
      dut.clock.step()
      dut.io_reset.rst_n.poke(true.B)

      dut.clock.step()
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(0.U)
      dut.io_instr.instr_rdata.poke(BigInt(addi, 16).U)
      dut.io_instr.instr_gnt.poke(true.B)

      dut.clock.step()
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(4.U)
      dut.io_instr.instr_rdata.poke(BigInt(add, 16).U)
      dut.io_instr.instr_gnt.poke(true.B)

      dut.io_rvfi.rvfi_valid.expect(true.B)
      dut.io_rvfi.rvfi_order.expect(0.U)
      dut.io_rvfi.rvfi_insn.expect(BigInt(addi, 16).U)
      dut.io_rvfi.rvfi_trap.expect(false.B)
      dut.io_rvfi.rvfi_halt.expect(false.B)
      dut.io_rvfi.rvfi_intr.expect(false.B)
      dut.io_rvfi.rvfi_mode.expect(0.U)
      dut.io_rvfi.rvfi_ixl.expect(0.U)

      dut.io_rvfi.rvfi_rs1_addr.expect(0.U)
      dut.io_rvfi.rvfi_rs2_addr.expect(0.U)
      dut.io_rvfi.rvfi_rs1_rdata.expect(0.U)
      dut.io_rvfi.rvfi_rs2_rdata.expect(0.U)

      dut.io_rvfi.rvfi_rd_addr.expect(1.U)
      dut.io_rvfi.rvfi_rd_wdata.expect(0x0ab.U)

      dut.io_rvfi.rvfi_pc_rdata.expect(0.U)
      dut.io_rvfi.rvfi_pc_wdata.expect(4.U)

      dut.clock.step()
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(8.U)
      dut.io_instr.instr_rdata.poke(0x00000013.U)
      dut.io_instr.instr_gnt.poke(true.B)

      dut.io_rvfi.rvfi_valid.expect(true.B)
      dut.io_rvfi.rvfi_order.expect(1.U)
      dut.io_rvfi.rvfi_insn.expect(BigInt(add, 16).U)
      dut.io_rvfi.rvfi_trap.expect(false.B)
      dut.io_rvfi.rvfi_halt.expect(false.B)
      dut.io_rvfi.rvfi_intr.expect(false.B)
      dut.io_rvfi.rvfi_mode.expect(0.U)
      dut.io_rvfi.rvfi_ixl.expect(0.U)

      dut.io_rvfi.rvfi_rs1_addr.expect(1.U)
      dut.io_rvfi.rvfi_rs2_addr.expect(1.U)
      dut.io_rvfi.rvfi_rs1_rdata.expect(0x0ab.U)
      dut.io_rvfi.rvfi_rs2_rdata.expect(0x0ab.U)

      dut.io_rvfi.rvfi_rd_addr.expect(2.U)
      dut.io_rvfi.rvfi_rd_wdata.expect(0x00000156.U)

      dut.io_rvfi.rvfi_pc_rdata.expect(4.U)
      dut.io_rvfi.rvfi_pc_wdata.expect(8.U)
    }
  }

  it should "branch" in {
    val beq = RISCVAssembler.fromString("beq x1, x2, 0x08").split("\n")(0)
    test(
      new Core(
        Seq(() =>
          new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
        )
      )
    ).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Reset the core
      dut.io_reset.boot_addr.poke(0.U)
      dut.io_reset.rst_n.poke(false.B)
      dut.io_instr.instr_gnt.poke(false.B)
      dut.io_data.data_gnt.poke(false.B)
      dut.clock.step()
      dut.io_reset.rst_n.poke(true.B)

      dut.clock.step()
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(0.U)
      dut.io_instr.instr_rdata.poke(BigInt(beq, 16).U)
      dut.io_instr.instr_gnt.poke(true.B)

      dut.clock.step()
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(0x08.U)
      dut.io_instr.instr_rdata.poke(0x00000013.U)
      dut.io_instr.instr_gnt.poke(true.B)

      dut.io_rvfi.rvfi_valid.expect(true.B)
      dut.io_rvfi.rvfi_order.expect(0.U)
      dut.io_rvfi.rvfi_insn.expect(BigInt(beq, 16).U)
      dut.io_rvfi.rvfi_trap.expect(false.B)
      dut.io_rvfi.rvfi_halt.expect(false.B)
      dut.io_rvfi.rvfi_intr.expect(false.B)
      dut.io_rvfi.rvfi_mode.expect(0.U)
      dut.io_rvfi.rvfi_ixl.expect(0.U)

      dut.io_rvfi.rvfi_rs1_addr.expect(1.U)
      dut.io_rvfi.rvfi_rs2_addr.expect(2.U)
      dut.io_rvfi.rvfi_rs1_rdata.expect(0.U)
      dut.io_rvfi.rvfi_rs2_rdata.expect(0.U)

      dut.io_rvfi.rvfi_rd_addr.expect(0.U)
      dut.io_rvfi.rvfi_rd_wdata.expect(0.U)

      dut.io_rvfi.rvfi_pc_rdata.expect(0.U)
      dut.io_rvfi.rvfi_pc_wdata.expect(0x08.U)

      dut.clock.step()
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(0x0c.U)
      dut.io_instr.instr_rdata.poke(0x00000013.U)
      dut.io_instr.instr_gnt.poke(true.B)

      dut.io_rvfi.rvfi_valid.expect(true.B)
      dut.io_rvfi.rvfi_order.expect(1.U)
      dut.io_rvfi.rvfi_insn.expect(0x00000013.U)
      dut.io_rvfi.rvfi_trap.expect(false.B)
      dut.io_rvfi.rvfi_halt.expect(false.B)
      dut.io_rvfi.rvfi_intr.expect(false.B)
      dut.io_rvfi.rvfi_mode.expect(0.U)
      dut.io_rvfi.rvfi_ixl.expect(0.U)

      dut.io_rvfi.rvfi_rs1_addr.expect(0.U)
      dut.io_rvfi.rvfi_rs2_addr.expect(0.U)
      dut.io_rvfi.rvfi_rs1_rdata.expect(0.U)
      dut.io_rvfi.rvfi_rs2_rdata.expect(0.U)

      dut.io_rvfi.rvfi_rd_addr.expect(0.U)
      dut.io_rvfi.rvfi_rd_wdata.expect(0.U)

      dut.io_rvfi.rvfi_pc_rdata.expect(0x08.U)
      dut.io_rvfi.rvfi_pc_wdata.expect(0x0c.U)
    }
  }

  it should "stall if no instruction is available" in {
    val nop = RISCVAssembler.fromString("nop").split("\n")(0)
    test(
      new Core(
        Seq(() =>
          new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
        )
      )
    ).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Reset the core
      dut.io_reset.boot_addr.poke(0.U)
      dut.io_reset.rst_n.poke(false.B)
      dut.io_instr.instr_gnt.poke(false.B)
      dut.io_data.data_gnt.poke(false.B)
      dut.clock.step()
      dut.io_reset.rst_n.poke(true.B)

      dut.clock.step()
      dut.io_rvfi.rvfi_valid.expect(false.B)
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(0.U)
      dut.io_instr.instr_rdata.poke(0.U)
      dut.io_instr.instr_gnt.poke(false.B)

      dut.clock.step()
      dut.io_rvfi.rvfi_valid.expect(false.B)
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(0.U)
      dut.io_instr.instr_rdata.poke(Integer.parseInt(nop, 16).U)
      dut.io_instr.instr_gnt.poke(true.B)

      dut.clock.step()
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(4.U)
      dut.io_instr.instr_rdata.poke(Integer.parseInt(nop, 16).U)
      dut.io_instr.instr_gnt.poke(true.B)

      dut.io_rvfi.rvfi_valid.expect(true.B)
      dut.io_rvfi.rvfi_order.expect(0.U)
      dut.io_rvfi.rvfi_insn.expect(Integer.parseInt(nop, 16).U)
      dut.io_rvfi.rvfi_trap.expect(false.B)
      dut.io_rvfi.rvfi_halt.expect(false.B)
      dut.io_rvfi.rvfi_intr.expect(false.B)
      dut.io_rvfi.rvfi_mode.expect(0.U)
      dut.io_rvfi.rvfi_ixl.expect(0.U)

      dut.io_rvfi.rvfi_rs1_addr.expect(0.U)
      dut.io_rvfi.rvfi_rs2_addr.expect(0.U)
      dut.io_rvfi.rvfi_rs1_rdata.expect(0.U)
      dut.io_rvfi.rvfi_rs2_rdata.expect(0.U)

      dut.io_rvfi.rvfi_rd_addr.expect(0.U)
      dut.io_rvfi.rvfi_rd_wdata.expect(0.U)

      dut.io_rvfi.rvfi_pc_rdata.expect(0.U)
      dut.io_rvfi.rvfi_pc_wdata.expect(4.U)

    }
  }

  it should "trap on a misaligned beq" in {
    val beq = RISCVAssembler.fromString("beq zero, zero, 6").split("\n")(0)
    val nop = RISCVAssembler.fromString("nop").split("\n")(0)
    test(
      new Core(
        Seq(() =>
          new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
        )
      )
    ).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Reset the core
      dut.io_reset.boot_addr.poke(0.U)
      dut.io_reset.rst_n.poke(false.B)
      dut.io_instr.instr_gnt.poke(false.B)
      dut.io_data.data_gnt.poke(false.B)
      dut.clock.step()
      dut.io_reset.rst_n.poke(true.B)

      dut.clock.step()
      dut.io_rvfi.rvfi_valid.expect(false.B)
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(0.U)
      dut.io_instr.instr_rdata.poke(0.U)
      dut.io_instr.instr_gnt.poke(false.B)

      dut.clock.step()
      dut.io_rvfi.rvfi_valid.expect(false.B)
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(0.U)
      dut.io_instr.instr_rdata.poke(Integer.parseInt(beq, 16).U)
      dut.io_instr.instr_gnt.poke(true.B)

      dut.clock.step()
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect("h00400000".U)
      dut.io_instr.instr_rdata.poke(Integer.parseInt(nop, 16).U)
      dut.io_instr.instr_gnt.poke(true.B)

      dut.io_rvfi.rvfi_valid.expect(true.B)
      dut.io_rvfi.rvfi_order.expect(0.U)
      dut.io_rvfi.rvfi_insn.expect(Integer.parseInt(beq, 16).U)
      dut.io_rvfi.rvfi_trap.expect(true.B)
      dut.io_rvfi.rvfi_halt.expect(false.B)
      dut.io_rvfi.rvfi_intr.expect(false.B)
      dut.io_rvfi.rvfi_mode.expect(0.U)
      dut.io_rvfi.rvfi_ixl.expect(0.U)

      dut.io_rvfi.rvfi_rs1_addr.expect(0.U)
      dut.io_rvfi.rvfi_rs2_addr.expect(0.U)
      dut.io_rvfi.rvfi_rs1_rdata.expect(0.U)
      dut.io_rvfi.rvfi_rs2_rdata.expect(0.U)

      dut.io_rvfi.rvfi_rd_addr.expect(0.U)
      dut.io_rvfi.rvfi_rd_wdata.expect(0.U)

      dut.io_rvfi.rvfi_pc_rdata.expect(0.U)
      dut.io_rvfi.rvfi_pc_wdata.expect("h00400000".U)

    }
  }

  it should "not trap on a non-taken misaligned bne" in {
    val bne = RISCVAssembler.fromString("bne zero, zero, 6").split("\n")(0)
    val nop = RISCVAssembler.fromString("nop").split("\n")(0)
    test(
      new Core(
        Seq(() =>
          new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
        )
      )
    ).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Reset the core
      dut.io_reset.boot_addr.poke(0.U)
      dut.io_reset.rst_n.poke(false.B)
      dut.io_instr.instr_gnt.poke(false.B)
      dut.io_data.data_gnt.poke(false.B)
      dut.clock.step()
      dut.io_reset.rst_n.poke(true.B)

      dut.clock.step()
      dut.io_rvfi.rvfi_valid.expect(false.B)
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(0.U)
      dut.io_instr.instr_rdata.poke(0.U)
      dut.io_instr.instr_gnt.poke(false.B)

      dut.clock.step()
      dut.io_rvfi.rvfi_valid.expect(false.B)
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(0.U)
      dut.io_instr.instr_rdata.poke(Integer.parseInt(bne, 16).U)
      dut.io_instr.instr_gnt.poke(true.B)

      dut.clock.step()
      dut.io_instr.instr_req.expect(true.B)
      dut.io_instr.instr_addr.expect(4.U)
      dut.io_instr.instr_rdata.poke(Integer.parseInt(nop, 16).U)
      dut.io_instr.instr_gnt.poke(true.B)

      dut.io_rvfi.rvfi_valid.expect(true.B)
      dut.io_rvfi.rvfi_order.expect(0.U)
      dut.io_rvfi.rvfi_insn.expect(Integer.parseInt(bne, 16).U)
      dut.io_rvfi.rvfi_trap.expect(false.B)
      dut.io_rvfi.rvfi_halt.expect(false.B)
      dut.io_rvfi.rvfi_intr.expect(false.B)
      dut.io_rvfi.rvfi_mode.expect(0.U)
      dut.io_rvfi.rvfi_ixl.expect(0.U)

      dut.io_rvfi.rvfi_rs1_addr.expect(0.U)
      dut.io_rvfi.rvfi_rs2_addr.expect(0.U)
      dut.io_rvfi.rvfi_rs1_rdata.expect(0.U)
      dut.io_rvfi.rvfi_rs2_rdata.expect(0.U)

      dut.io_rvfi.rvfi_rd_addr.expect(0.U)
      dut.io_rvfi.rvfi_rd_wdata.expect(0.U)

      dut.io_rvfi.rvfi_pc_rdata.expect(0.U)
      dut.io_rvfi.rvfi_pc_wdata.expect(4.U)

    }
  }
}
