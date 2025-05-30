package project1.public.RISCV.RV32I

import RISCV.implementation.Core
import RISCV.implementation.RV32I._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import project1.utils.ProcessorTestUtils._
import project1.utils.RISCVInstruction
import project1.utils.RISCVInstruction.getSignedValue
import project1.utils.RISCVInstruction.signedToUnsigned
import project1.utils.models._

class SingleInstructionTests
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers {

  def instructions: Seq[() => ProcessorState => RISCVInstruction] = Seq(
    (() => RISCVInstruction.getADDI(_)),
    (() => RISCVInstruction.getSLTI(_)),
    (() => RISCVInstruction.getSLTIU(_)),
    (() => RISCVInstruction.getANDI(_)),
    (() => RISCVInstruction.getORI(_)),
    (() => RISCVInstruction.getXORI(_)),
    (() => RISCVInstruction.getSLLI(_)),
    (() => RISCVInstruction.getSRLI(_)),
    (() => RISCVInstruction.getSRAI(_)),
    (() => RISCVInstruction.getLUI(_)),
    (() => RISCVInstruction.getAUIPC(_)),
    (() => RISCVInstruction.getADD(_)),
    (() => RISCVInstruction.getSUB(_)),
    (() => RISCVInstruction.getSLL(_)),
    (() => RISCVInstruction.getSLT(_)),
    (() => RISCVInstruction.getSLTU(_)),
    (() => RISCVInstruction.getXOR(_)),
    (() => RISCVInstruction.getSRL(_)),
    (() => RISCVInstruction.getSRA(_)),
    (() => RISCVInstruction.getOR(_)),
    (() => RISCVInstruction.getAND(_)),
    (() => RISCVInstruction.getJAL(_)),
    (() => RISCVInstruction.getJALR(_)),
    (() => RISCVInstruction.getBEQ(_)),
    (() => RISCVInstruction.getBNE(_)),
    (() => RISCVInstruction.getBLT(_)),
    (() => RISCVInstruction.getBGE(_)),
    (() => RISCVInstruction.getBLTU(_)),
    (() => RISCVInstruction.getBGEU(_)),
    (() => RISCVInstruction.getLB(_)),
    (() => RISCVInstruction.getLH(_)),
    (() => RISCVInstruction.getLW(_)),
    (() => RISCVInstruction.getLBU(_)),
    (() => RISCVInstruction.getLHU(_)),
    (() => RISCVInstruction.getSB(_)),
    (() => RISCVInstruction.getSH(_)),
    (() => RISCVInstruction.getSW(_))
  )

  for (instr <- instructions) {
    val name = instr()(new ProcessorState).instruction.split(" ")(0)
    it should "do a random " + name in {
      test(
        new Core(
          Seq(() =>
            new RV32I(new ControlUnit, new Decoder, new BranchUnit, new ALU)
          )
        )
      ).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        val registers = generateRandomMap(
          signedToUnsigned(getSignedValue(0xffffffff, 32), 32)
        )
        resetCore(dut)
        val state = prepareState(dut, registers)
        val instruction = instr()(state)
        executeInstruction(
          dut,
          instruction.assembly,
          if (instruction.state != null) instruction.state else state
        )
        evaluateRVFI(dut, instruction.effect)
      }
    }
  }

}
