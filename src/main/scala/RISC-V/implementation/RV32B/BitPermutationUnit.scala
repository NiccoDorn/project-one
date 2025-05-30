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

  io.stall := STALL_REASON.NO_STALL

  io_data <> DontCare
  io_reg <> DontCare
  io_pc <> DontCare
  io_reset <> DontCare
  io_trap <> DontCare

  generalizedReverser.io <> DontCare
  shuffler.io <> DontCare
  rotater.io <> DontCare

  ??? // TODO: implement Task 2.5 here

}
