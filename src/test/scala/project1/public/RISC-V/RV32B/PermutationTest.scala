package project1.public.RISCV.RV32B

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import RISCV.utils.PermBuilder.buildPermutation
import org.scalatest.time._


import java.io.{File, PrintWriter} // for writing generated instructions to file


class PermutationTest extends AnyFlatSpec with Matchers {

    behavior of "CustomPermutation"

    def writeInstructionsToFile(filename: String, instructions: List[String]): Unit = {
        val directory = new File("generated_instructions")
        if (!directory.exists()) {
            directory.mkdirs()
        }
        val file = new File(directory, s"$filename.txt")
        val writer = new PrintWriter(file)
        try {
            instructions.foreach(writer.println)
            println(s"Instructions for '$filename' written to ${file.getAbsolutePath}")
        } finally {
            writer.close()
        }
    }

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
        permutation.zipWithIndex.map(_.swap).toMap

    }

    it should "perform a simple rotation correctly" in {
        val permutation = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
        16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 0).zipWithIndex.map(_.swap).toMap
        val instructions = buildPermutation(1, 2, permutation)
        instructions should not be empty
        writeInstructionsToFile("01_simple_rotation", instructions)
        val result = emulatePermutation(1, 2, instructions)
        result shouldEqual permutation
    }

    it should "perform a bit reversal permutation correctly" in {
        val permutation = Map(
            0 -> 31, 1 -> 30, 2 -> 29, 3 -> 28, 4 -> 27, 5 -> 26, 6 -> 25, 7 -> 24,
            8 -> 23, 9 -> 22, 10 -> 21, 11 -> 20, 12 -> 19, 13 -> 18, 14 -> 17, 15 -> 16,
            16 -> 15, 17 -> 14, 18 -> 13, 19 -> 12, 20 -> 11, 21 -> 10, 22 -> 9, 23 -> 8,
            24 -> 7, 25 -> 6, 26 -> 5, 27 -> 4, 28 -> 3, 29 -> 2, 30 -> 1, 31 -> 0
        )

        val instructions = buildPermutation(1, 2, permutation)
        instructions should not be empty
        writeInstructionsToFile("02_bit_reversal_permutation", instructions)
        val result = emulatePermutation(1, 2, instructions)
        result shouldEqual permutation
    }

    it should "perform a byte swap permutation correctly" in {
        val permutation = Map(
            0 -> 24, 1 -> 25, 2 -> 26, 3 -> 27, 4 -> 28, 5 -> 29, 6 -> 30, 7 -> 31,
            8 -> 16, 9 -> 17, 10 -> 18, 11 -> 19, 12 -> 20, 13 -> 21, 14 -> 22, 15 -> 23,
            16 -> 8, 17 -> 9, 18 -> 10, 19 -> 11, 20 -> 12, 21 -> 13, 22 -> 14, 23 -> 15,
            24 -> 0, 25 -> 1, 26 -> 2, 27 -> 3, 28 -> 4, 29 -> 5, 30 -> 6, 31 -> 7
        )

        val instructions = buildPermutation(1, 2, permutation)
        instructions should not be empty
        writeInstructionsToFile("03_byte_swap_permutation", instructions)
        val result = emulatePermutation(1, 2, instructions)
        result shouldEqual permutation
    }

    it should "perform a nibble swap permutation correctly" in {
        val permutation = Map(
            0 -> 4, 1 -> 5, 2 -> 6, 3 -> 7, 4 -> 0, 5 -> 1, 6 -> 2, 7 -> 3,
            8 -> 12, 9 -> 13, 10 -> 14, 11 -> 15, 12 -> 8, 13 -> 9, 14 -> 10, 15 -> 11,
            16 -> 20, 17 -> 21, 18 -> 22, 19 -> 23, 20 -> 16, 21 -> 17, 22 -> 18, 23 -> 19,
            24 -> 28, 25 -> 29, 26 -> 30, 27 -> 31, 28 -> 24, 29 -> 25, 30 -> 26, 31 -> 27
        )

        val instructions = buildPermutation(1, 2, permutation)
        instructions should not be empty
        writeInstructionsToFile("04_nibble_swap_permutation", instructions)
        val result = emulatePermutation(1, 2, instructions)
        result shouldEqual permutation
    }

    it should "perform a generalized reverse correctly" in {
        val permutation = List(1, 0, 3, 2, 5, 4, 7, 6, 9, 8, 11, 10, 13, 12, 15,
            14, 17, 16, 19, 18, 21, 20, 23, 22, 25, 24, 27, 26, 29, 28, 31, 30).zipWithIndex.map(_.swap).toMap
        val instructions = buildPermutation(1, 2, permutation)
        instructions should not be empty
        writeInstructionsToFile("05_generalized_reverse", instructions)
        val result = emulatePermutation(1, 2, instructions)
        result shouldEqual permutation
    }

    it should "perform a shuffle correctly" in {
        val permutation = List(0, 2, 1, 3, 4, 6, 5, 7, 8, 10, 9, 11, 12, 14, 13,
            15, 16, 18, 17, 19, 20, 22, 21, 23, 24, 26, 25, 27, 28, 30, 29, 31).zipWithIndex.map(_.swap).toMap
        val instructions = buildPermutation(1, 2, permutation)
        instructions should not be empty
        writeInstructionsToFile("06_shuffle", instructions)
        val result = emulatePermutation(1, 2, instructions)
        result shouldEqual permutation
    }

    it should "perform grevi 0x1 (pairwise adjacent swap) correctly" in {
        val targetList = List(
            1, 0, 3, 2, 5, 4, 7, 6, 9, 8, 11, 10, 13, 12, 15, 14,
            17, 16, 19, 18, 21, 20, 23, 22, 25, 24, 27, 26, 29, 28, 31, 30
        )
        val permutation = targetList.zipWithIndex.map(_.swap).toMap
        val instructions = buildPermutation(1, 2, permutation)
        instructions should not be empty
        writeInstructionsToFile("07_grevi_0x1_pairwise_swap", instructions)
        val result = emulatePermutation(1, 2, instructions)
        result shouldEqual permutation
    }

    it should "perform reverse of first 16 and last 16 bits correctly" in {
        val firstHalfReversed = (0 until 16).reverse.toList
        val secondHalfReversed = (16 until 32).reverse.toList
        val targetList = firstHalfReversed ++ secondHalfReversed
        val permutation: Map[Int, Int] = targetList.zipWithIndex.map(_.swap).toMap
        val instructions = buildPermutation(1, 2, permutation)
        instructions should not be empty
        writeInstructionsToFile("08_reverse_first_and_last_16_bits", instructions)
        val result = emulatePermutation(1, 2, instructions)
        result shouldEqual permutation
    }

    it should "perform reverse of first 16 and last 16 bits followed by pairwise swap correctly" in {
        val firstHalfReversed = (0 until 16).reverse.toList // (15, 14, ..., 0)
        val secondHalfReversed = (16 until 32).reverse.toList // (31, 30, ..., 16)
        val intermediatePermutation = firstHalfReversed ++ secondHalfReversed
        val finalExpectedPermutationList = performGeneralizedReverse(intermediatePermutation, 0x1)
        val permutation: Map[Int, Int] = finalExpectedPermutationList.zipWithIndex.map(_.swap).toMap
        val instructions = buildPermutation(1, 2, permutation)
        instructions should not be empty
        writeInstructionsToFile("09_reverse_16_bit_and_pairwise_swap", instructions)
        val result = emulatePermutation(1, 2, instructions)
        result shouldEqual permutation
    }

    it should "perform bit reversal within each 8-bit byte correctly" in {
        val targetList = (0 until 4).flatMap { byteIdx =>
            val startBit = byteIdx * 8
            val endBit = startBit + 8
            (startBit until endBit).reverse
        }.toList
        val permutation: Map[Int, Int] = targetList.zipWithIndex.map(_.swap).toMap

        val instructions = buildPermutation(1, 2, permutation)
        instructions should not be empty
        writeInstructionsToFile("10_bit_reversal_per_byte", instructions)
        val result = emulatePermutation(1, 2, instructions)
        result shouldEqual permutation
    }

    it should "perform a 2-bit block swap (chessboard pattern) correctly" in {
        val targetList = (0 until 32).map { i =>
            val blockIndex = i / 2
            val bitInBlock = i % 2
            if (blockIndex % 2 == 0) {
                (blockIndex + 1) * 2 + bitInBlock
            } else {
                (blockIndex - 1) * 2 + bitInBlock
            }
        }.toList
        val permutation: Map[Int, Int] = targetList.zipWithIndex.map(_.swap).toMap
        val instructions = buildPermutation(1, 2, permutation)
        instructions should not be empty
        writeInstructionsToFile("11_2_bit_block_swap_chessboard", instructions)
        val result = emulatePermutation(1, 2, instructions)
        result shouldEqual permutation
    }

    it should "perform 1-4 swap correctly" in {
        
        withClue("test for 1-4 swap exceeded timeout") {
        }
        val initialList = List.range(0, 32)
        val targetList: List[Int] = initialList
            .updated(1, 4)
            .updated(4, 1)
        val permutation: Map[Int, Int] = targetList.zipWithIndex.map(_.swap).toMap
        val startTime = System.nanoTime()
        val instructions = buildPermutation(1, 2, permutation)
        val endTime = System.nanoTime() // End timer
        val duration = (endTime - startTime) / 1_000_000.0
        println(s"duration of buildPermutation for test 'perform 1-4 swap correctly': $duration ms")
        instructions should not be empty
        writeInstructionsToFile("12_1_4_swap", instructions)
        val result = emulatePermutation(1, 2, instructions)
        result shouldEqual permutation
    }

    it should "demonstrate timeout (mock)" in {
        Thread.sleep(3000)
        1 shouldEqual 1
    }

    // A test that will intentionally timeout if limit is 1 second
    it should "demonstrate intentional timeout (mock)" in { // Corrected: Removed `taggedAs(Slow)`
        Thread.
        sleep(2000)
        1 shouldEqual 1
    }
    
    it should "perform pairwise swaps at word ends (0,1 and 30,31) correctly" in {
        val initialList = List.range(0, 32)
        val targetList: List[Int] = initialList
            .updated(0, 1)
            .updated(1, 0)
            .updated(30, 31)
            .updated(31, 30)
        val permutation: Map[Int, Int] = targetList.zipWithIndex.map(_.swap).toMap
        val instructions = buildPermutation(1, 2, permutation)
        instructions should not be empty
        writeInstructionsToFile("13_LSBs_MSBs_swaps", instructions)
        val result = emulatePermutation(1, 2, instructions)
        result shouldEqual permutation
    }
}
