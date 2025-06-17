package RISCV.implementation.RV32B

import chisel3._
import chisel3.util._

import RISCV.interfaces.generic.AbstractExecutionUnit
import RISCV.model._
import bitmanipulation.AbstractGeneralizedReverser
import bitmanipulation.AbstractShuffler
import bitmanipulation.AbstractSequentialRotater

class BitPermutationUnit(
    genGeneralizedReverser: () => AbstractGeneralizedReverser,
    genShuffler: () => AbstractShuffler,
    genRotater: () => AbstractSequentialRotater
) extends AbstractExecutionUnit(InstructionSets.BitPerm) {

  io.misa := "b01__0000__0_00000_00000_00000_00000_00010".U

  val generalizedReverser = Module(genGeneralizedReverser())
  val shuffler = Module(genShuffler())
  val rotater = Module(genRotater())

  io_data <> DontCare
  io_reset <> DontCare
  io_trap <> DontCare

  generalizedReverser.io <> DontCare
  shuffler.io <> DontCare
  rotater.io <> DontCare

  /* Task 2.5 impl */

  /* get the full sequence to later check which instruction is wanted */
  // [31...0] - using correct signal name io.instr instead of io.instruction
  val op = io.instr(6,0)
  val funct3 = io.instr(14, 12)
  val funct5 = io.instr(31,27)
  val funct7 = io.instr(31, 25)
  val rd = io.instr(11, 7)
  val rs1 = io.instr(19, 15)
  val rs2 = io.instr(24, 20)

  /* get correct instruction from encoded bit sequence */
  val grev  = funct3 === "b101".U && funct7 === "b0110100".U && op === "b0110011".U
  val grevi = funct3 === "b101".U && funct5 === "b01101".U   && op === "b0010011".U

  val shfl  = funct3 === "b001".U && funct7 === "b0000100".U && op === "b0110011".U
  val shfli = funct3 === "b001".U && funct5 === "b00001".U && op === "b0010011".U

  val unshfl  = funct3 === "b101".U && funct7 === "b0000100".U && op === "b0110011".U
  val unshfli = funct3 === "b101".U && funct5 === "b00001".U && op === "b0010011".U

  val rol  = funct3 === "b001".U && funct7 === "b0110000".U && op === "b0110011".U
  val ror  = funct3 === "b101".U && funct7 === "b0110000".U && op === "b0110011".U
  val rori = funct3 === "b101".U && funct5 === "b01100".U && op === "b0010011".U

  val isImmInstr = grevi || shfli || unshfli || rori
  val validInstr = io.valid && (grev || grevi || shfl || shfli || unshfl || unshfli || rol || ror || rori)
println("checkpoint1")
  /* Let the speculative execution commence */
  val rs1_data = io_reg.reg_read_data1
  val rs2_data = io_reg.reg_read_data2
  val imm = io.instr(26, 20)
  // exec reverser, shuffler and rotator speculatively
  generalizedReverser.io.input := rs1_data
  generalizedReverser.io.pattern := Mux(grevi, imm, rs2_data)
  
  shuffler.io.input := rs1_data
  shuffler.io.pattern := Mux(shfli || unshfli, imm, rs2_data)
  shuffler.io.unshuffle := (unshfl || unshfli).asUInt
  
  rotater.io.input := rs1_data
  rotater.io.shamt := Mux(rori, imm, rs2_data)
  rotater.io.start := io.valid && (rol || ror || rori)

  val grevRes = generalizedReverser.io.result
  val shflRes = shuffler.io.result
  val rotRes = rotater.io.result
println("checkpoint2")
  /* get true instruction result using MuxCase - *plopp* noice! */
  val result = MuxCase(0.U, Seq(
    (grev || grevi) -> grevRes,
    (shfl || shfli || unshfl || unshfli) -> shflRes,
    (rol || ror || rori) -> rotRes
  ))

  // stall for sequential rotation operations that are not done
  val isRotOp = rol || ror || rori
  val shouldStall = validInstr && isRotOp && !rotater.io.done
  io.stall := Mux(shouldStall, STALL_REASON.EXECUTION_UNIT, STALL_REASON.NO_STALL)
  
println("checkpoint3")
  // write back if valid and not stalled
  io_reg.reg_write_en := validInstr && !shouldStall
  io_reg.reg_rd := rd
  io_reg.reg_write_data := result
  io_reg.reg_rs1 := rs1
  
println("checkpoint4")
  // if immediate then r2 is 0
  when(grev) {
    io_reg.reg_rs2 := rs2
  }.otherwise{
    io_reg.reg_rs2 := Mux(isImmInstr, 0.U, rs2)
  }

println("checkpoint5")
  // increment program counter by + 4
  io_pc.pc_we := validInstr && !shouldStall
  io_pc.pc_wdata := io_pc.pc + 4.U
println("checkpoint6")
}