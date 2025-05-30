package project1.public.RISCV

import RISCV.implementation._
import RISCV.utils.assembler.RISCVAssembler
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import project1.utils.ProcessorTestUtils._
import project1.utils.models._

import java.io.File
import scala.io.Source
import scala.util.Try

import collection.mutable

// sbt "testOnly project1.RISCV.ProcessorTest"

class ProcessorTest
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers {

  val onlyRun =
    "" // Set to "" to run all tests, or set to a specific test name to run only that test
  //            e.g. "public/permutation/swap"

  def runProgram(folderName: String): Unit = {
    val config: TestConfig = TestConfig.fromJson(
      Source.fromResource(s"project1/$folderName/config.json").mkString
    ) match {
      case Right(value) => value
      case Left(error)  => throw new Exception(error)
    }
    test(
      new Core(
        config.modules
      )
    ).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      var state = new ProcessorState()

      resetCore(dut)
      state = prepareState(
        dut,
        config.initial_reg.map(x => x._1.toInt -> x._2.toLong).toMap
      )
      state.data_mem = config.initial_mem
      state.instr_mem = RISCVAssembler
        .fromString(
          Source.fromResource(s"project1/$folderName/program.s").mkString
        )
        .split("\n")
        .map(line => BigInt(line, 16).U)
        .zipWithIndex
        .map(x => BigInt(x._2 * 4 + state.pc) -> x._1.litValue)
        .to(mutable.Map)
      val instrCount = if (config.executed_instructions == 0) {
        state.instr_mem.size
      } else {
        config.executed_instructions.toInt
      }
      for (_ <- 0 until instrCount by 1) {
        val instr = Try(state.instr_mem(state.pc))
          .getOrElse(
            throw new Exception(s"Instruction not found at PC: ${state.pc}")
          )
        state = executeInstruction(dut, instr.U, state)
      }
      config.final_reg.foreach(x => assert(state.registers(x._1) == x._2))
      config.final_mem.foreach(x => assert(state.data_mem(x._1) == x._2))
    }
  }

  if (onlyRun != "") {
    it should "only run one test" in {
      runProgram(onlyRun)
    }
  } else {
    val test_type = "public"
    val test_categories = new File(s"src/test/resources/project1/$test_type")
      .listFiles()
      .map(_.getName)
    for (test_category <- test_categories) {
      val test_names = new File(
        s"src/test/resources/project1/$test_type/$test_category"
      ).listFiles().map(_.getName)
      for (test_name <- test_names) {
        it should s"${test_type}_${test_category}_${test_name}" in {
          runProgram(s"$test_type/$test_category/$test_name")
        }
      }
    }
  }
}
