package bitmanipulation

import chisel3._
import chisel3.util._

abstract class AbstractLeadingZerosCounter(bitWidth: Int) extends Module {
  val io = IO(new Bundle {
    val input = Input(UInt(bitWidth.W))
    val result = Output(UInt(log2Ceil(bitWidth + 1).W))
  })
}

// You may expect bitWidth to be a power of two.
class LeadingZerosCounter(bitWidth: Int)
    extends AbstractLeadingZerosCounter(bitWidth) {

  // Simple assertions
  assert(bitWidth > 0 && isPow2(bitWidth))


  if (bitWidth == 1){
    io.result := Mux(io.input === 0.U, 1.U, 0.U)
  }
  else{
    val mid = bitWidth / 2
    val lower = io.input(mid - 1, 0)
    val upper = io.input(bitWidth - 1, mid)

    // Create lower submodule
    val countLower = Module(new LeadingZerosCounter(mid))
    countLower.io.input := lower
    val resultLower = countLower.io.result
    
    // Create upper submodule
    val countUpper = Module(new LeadingZerosCounter(mid))
    countUpper.io.input := upper
    val resultUpper = countUpper.io.result


    /* If upper is zero then just add all the upper bits and do only lower submodule, 
      otherwise we need only upper submodule because the leading zeroes end somewhere in that part */
    val isUpperZero = (upper === 0.U)
    val sumUpperLower = resultLower + mid.U
    val countTotal = Mux(isUpperZero, sumUpperLower, resultUpper)


    /* End of the recursion step. Basically if the input is zero then simply return the bit width
      otherwise we return the result we counted so far. */
    val isTotalZero = (io.input === 0.U)
    io.result := Mux(isTotalZero, bitWidth.U, countTotal)



  }

  


}
