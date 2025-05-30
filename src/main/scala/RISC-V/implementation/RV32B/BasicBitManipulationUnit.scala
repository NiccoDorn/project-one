package RISCV.implementation.RV32B

import chisel3._
import chisel3.util._

import RISCV.interfaces.generic.AbstractExecutionUnit
import RISCV.model._
import bitmanipulation.AbstractLeadingZerosCounter

class BasicBitManipulationUnit(
    genLeadingZerosCounter: () => AbstractLeadingZerosCounter
) extends AbstractExecutionUnit(InstructionSets.BasicBit) {
  io.misa := "b01__0000__0_00000_00000_00000_00000_00010".U

  val leadingZerosCounter = Module(genLeadingZerosCounter())

  io.stall := STALL_REASON.NO_STALL

  io_data <> DontCare
  io_reg <> DontCare
  io_pc <> DontCare
  io_reset <> DontCare
  io_trap <> DontCare

  leadingZerosCounter.io <> DontCare

  ??? // TODO: implement Task 2.4 here

}
