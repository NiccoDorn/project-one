package bitmanipulation

import chisel3._
import chisel3.util._

abstract class AbstractShuffler(bitWidth: Int) extends Module {
  val io = IO(new Bundle {
    val input = Input(UInt(bitWidth.W))
    val pattern = Input(UInt((log2Ceil(bitWidth) - 1).W))
    val unshuffle = Input(UInt(1.W))
    val result = Output(UInt(bitWidth.W))
  })
}

class Shuffler(bitWidth: Int) extends AbstractShuffler(bitWidth) {
  // IDEA:
  // If inv = 0: split => swap recursive result => return
  // If inv = 1: swap 2/2 and 3/4 => split => recurse => return


  assert(isPow2(bitWidth), s"Bit width must be power of two")

  if(bitWidth == 4) {
    //  4-bits => swap the middle 2 bits iff. pattern(0)==1
    when (io.pattern(0) === 1.U){
      io.result := Cat(io.input(3),io.input(1),io.input(2),io.input(0))
    }otherwise{
      io.result := io.input
    }
  } else if(bitWidth<4) {
    // <4-bits => no change
    // idk what is defined but i think it makes no sense to change anything
    io.result := io.input

  }else{
    // >4-bits => see IDEA
    //split
    val mid = bitWidth/2
    val upper_in = io.input(bitWidth-1, mid)
    val lower_in = io.input(mid-1, 0)

    val len_pattern = log2Ceil(bitWidth) - 1

    //swap 2/4 with 3/4 of input; i.e shuffle the inputs
    val upper_shuffled_in = Cat(upper_in(mid-1,mid/2),lower_in(mid-1,mid/2))
    val lower_shuffled_in = Cat(upper_in(mid/2-1,0),lower_in(mid/2-1,0))

    val upper_rec = Mux(!io.unshuffle(0),upper_shuffled_in,upper_in)
    val lower_rec = Mux(!io.unshuffle(0),lower_shuffled_in,lower_in)

    //generate result of the recursive step
    val upper_shuffle = Module(new Shuffler(mid))
    upper_shuffle.io.input := upper_rec
    upper_shuffle.io.unshuffle := io.unshuffle
    upper_shuffle.io.pattern := io.pattern(len_pattern-2,0)
    val upper_out = upper_shuffle.io.result
    
    val lower_shuffle = Module(new Shuffler(mid))
    lower_shuffle.io.input := lower_rec
    lower_shuffle.io.unshuffle := io.unshuffle
    lower_shuffle.io.pattern := io.pattern(len_pattern-2,0)
    val lower_out = lower_shuffle.io.result

    val shuffled_out = Cat(upper_out,lower_out)

    //swap 2/4 with 3/4 of rec-result; i.e shuffle the recursive result
    val shuffled_res = Cat(upper_out(mid-1,mid/2),lower_out(mid-1,mid/2),upper_out(mid/2-1,0),lower_out(mid/2-1,0))

    val swap = io.pattern(len_pattern-1)
    io.result:= Mux(swap,Mux(!io.unshuffle(0),shuffled_out,shuffled_res),shuffled_out)
    
  }
}

