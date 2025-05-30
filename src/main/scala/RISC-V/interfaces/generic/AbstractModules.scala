package RISCV.interfaces.generic

import chisel3._
import RISCV.model.RISCV_TYPE

abstract class AbstractExecutionUnit(instr_set: Seq[RISCV_TYPE.Type]) extends Module {
  val io = IO(new ExecutionUnitInterface)
  val io_reset = IO(new ResetInterface)
  val io_pc = IO(new PCInterface)
  val io_reg = IO(new RegisterInterface)
  val io_data = IO(new DataInterface)
  val io_trap = IO(new TrapInterface)

  val instr_type = io.instr_type

  val valid_instr = VecInit(instr_set.map(typ => typ.asUInt).toSeq)
  io.valid := valid_instr.contains(instr_type.asUInt)
}

abstract class AbstractProgramCounter extends Module {
  val io = IO(Flipped(new PCInterface))
  val io_reset = IO(new ResetInterface)
}

abstract class AbstractRegisterFile extends Module{
    val io = IO(Flipped(new RegisterInterface))
    val io_reset = IO(new ResetInterface)
}
