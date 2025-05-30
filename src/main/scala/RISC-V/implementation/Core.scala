package RISCV.implementation


import RISCV.interfaces.generic.AbstractExecutionUnit
import RISCV.implementation.generic._

class Core (genExecutionUnits : Seq[() => AbstractExecutionUnit]) extends GenericCore(genExecutionUnits, new ProgramCounter, new RegisterFile) {
  // Nothing to do here.
}
