package project1.public.RISCV.RV32B

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import RISCV.utils.PermBuilder.buildPermutation

class PermutationTest extends AnyFlatSpec  with Matchers {

    behavior of "CustomPermutation"

    def performRotation(
        permutation: List[Int],
        immediate: Int
    ): List[Int] = {
        val n = immediate & 31
        permutation.drop(n) ++ permutation.take(n)
    }

    def performGeneralizedReverse(
        permutation: List[Int],
        immediate: Int
    ): List[Int] = {
        var new_permutation = permutation
        for (i <- 0 until 32) yield {
            var old_index = i
            if ((immediate & (1 << 4)) != 0) old_index = if (old_index >= 16) old_index - 16 else old_index + 16
            if ((immediate & (1 << 3)) != 0) old_index = if (old_index % 16 >= 8) old_index - 8 else old_index + 8
            if ((immediate & (1 << 2)) != 0) old_index = if (old_index % 8 >= 4) old_index - 4 else old_index + 4
            if ((immediate & (1 << 1)) != 0) old_index = if (old_index % 4 >= 2) old_index - 2 else old_index + 2
            if ((immediate & (1 << 0)) != 0) old_index = if (old_index % 2 == 1) old_index - 1 else old_index + 1
        
            new_permutation = new_permutation.updated(
                i,
                permutation(old_index)
            )
        }
        new_permutation


    }

    def performShuffle(
        permutation: List[Int],
        immediate: Int
    ): List[Int] = {
        var new_permutation = permutation
        for (i <- 0 until 32) yield {
            var old_index = i
            if ((immediate & (1 << 0)) != 0) old_index = old_index % 4 match {
                case 0 => old_index
                case 1 => old_index + 1
                case 2 => old_index - 1
                case 3 => old_index 
            }
            if ((immediate & (1 << 1)) != 0) old_index = (old_index % 8 / 2) match {
                case 0 => old_index
                case 1 => old_index + 2
                case 2 => old_index - 2
                case 3 => old_index 
            }
            if ((immediate & (1 << 2)) != 0) old_index = (old_index % 16 / 4) match {
                case 0 => old_index
                case 1 => old_index + 4
                case 2 => old_index - 4
                case 3 => old_index 
            }
            if ((immediate & (1 << 3)) != 0) old_index = (old_index % 32 / 8) match {
                case 0 => old_index
                case 1 => old_index + 8
                case 2 => old_index - 8
                case 3 => old_index 
            }
            new_permutation = new_permutation.updated(
                i,
                permutation(old_index)
            )
        }
        new_permutation
    }

    def performUnshuffle(
        permutation: List[Int],
        immediate: Int
    ): List[Int] = {
        var new_permutation = permutation
        for (i <- 0 until 32) yield {
            var old_index = i
            if ((immediate & (1 << 3)) != 0) old_index = (old_index % 32 / 8) match {
                case 0 => old_index
                case 1 => old_index + 8
                case 2 => old_index - 8
                case 3 => old_index 
            }
            if ((immediate & (1 << 2)) != 0) old_index = (old_index % 16 / 4) match {
                case 0 => old_index
                case 1 => old_index + 4
                case 2 => old_index - 4
                case 3 => old_index 
            }
            if ((immediate & (1 << 1)) != 0) old_index = (old_index % 8 / 2) match {
                case 0 => old_index
                case 1 => old_index + 2
                case 2 => old_index - 2
                case 3 => old_index 
            }
            if ((immediate & (1 << 0)) != 0) old_index = old_index % 4 match {
                case 0 => old_index
                case 1 => old_index + 1
                case 2 => old_index - 1
                case 3 => old_index 
            }
            new_permutation = new_permutation.updated(
                i,
                permutation(old_index)
            )
        }
        new_permutation
        
    }

    def parseImmediate(imm: String): Int = {
    try {
      if (imm.startsWith("0x")) BigInt(imm.substring(2), 16).toInt
      else imm.toInt
    } catch {
      case _: Exception => throw new Exception(s"Invalid immediate value ${imm}.")
    }
  }

    def emulatePermutation(
        rd: Int,
        rs1: Int,
        instructions: List[String]
    ): Map[Int, Int] = {
        var permutation = List.range(0, 32)
        for (instruction <- instructions) {
            if (instruction.split(" ").length != 4) {
                throw new IllegalArgumentException(s"Invalid instruction format: $instruction")
            }
            if (instruction.split(" ")(1) != s"x$rd" && instruction.split(" ")(1) != s"x$rd,") {
                throw new IllegalArgumentException(s"Invalid destination register. Make sure to use \"x$rd\": $instruction")
            }
            if (instruction.split(" ")(2) != s"x$rs1" && instruction.split(" ")(2) != s"x$rs1," && instruction.split(" ")(2) != s"x$rd" && instruction.split(" ")(2) != s"x$rd," ) {
                throw new IllegalArgumentException(s"Invalid source register. Make sure to use \"x$rs1\" or \"x$rd\": $instruction")
            }
            val opcode = instruction.split(" ")(0)
            val immediate = parseImmediate(instruction.split(" ")(3))
            permutation = opcode match {
                case "rori" => performRotation(permutation, immediate)
                case "roli" => performRotation(permutation, 32 - immediate % 32)
                case "grevi" => performGeneralizedReverse(permutation, immediate)
                case "shfli" => performShuffle(permutation, immediate)
                case "unshfli" => performUnshuffle(permutation, immediate)
                case _ => throw new IllegalArgumentException(s"Unknown instruction: $opcode")
            }
        }
        permutation.zipWithIndex.toMap

    }

    it should "perform a simple rotation correctly" in {
        val permutation = List(1, 0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
            16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31).zipWithIndex.map(_.swap).toMap
        val instructions = buildPermutation(1, 2, permutation)
        val result = emulatePermutation(1, 2, instructions)
        result shouldEqual permutation
    }

    it should "perform a generalized reverse correctly" in {
        val permutation = List(1, 0, 3, 2, 5, 4, 7, 6, 9, 8, 11, 10, 13, 12, 15,
            14, 17, 16, 19, 18, 21, 20, 23, 22, 25, 24, 27, 26, 29, 28, 31, 30).zipWithIndex.map(_.swap).toMap
        val instructions = buildPermutation(1, 2, permutation)
        val result = emulatePermutation(1, 2, instructions)
        result shouldEqual permutation
    }

    it should "perform a shuffle correctly" in {
        val permutation = List(0, 2, 1, 3, 4, 6, 5, 7, 8, 10, 9, 11, 12, 14, 13,
            15, 16, 18, 17, 19, 20, 22, 21, 23, 24, 26, 25, 27, 28, 30, 29, 31).zipWithIndex.map(_.swap).toMap
        val instructions = buildPermutation(1, 2, permutation)
        val result = emulatePermutation(1, 2, instructions)
        result shouldEqual permutation
    }
}