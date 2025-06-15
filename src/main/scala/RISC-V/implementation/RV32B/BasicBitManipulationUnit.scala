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

  /* get the full sequence to later check which instruction is wanted */
  // [31...0] - using correct signal name io.instr instead of io.instruction
  val op = io.instr(6,0)
  val funct3 = io.instr(14, 12)
  val funct7 = io.instr(31, 25)
  val rd = io.instr(11, 7)
  val rs1 = io.instr(19, 15)
  val rs2 = io.instr(24, 20)

  /* Let the speculative execution commence */
  val rs1_data = io_reg.reg_read_data1
  val rs2_data = io_reg.reg_read_data2
  
  val bitWidth = rs1_data.getWidth // assumes both inputs have same bitWidth

/* get correct instruction from encoded bit sequence */
  val min = funct3 === "b100".U && funct7 === "b0000101".U  && op === "b0010011".U
  val minu = funct3 === "b101".U && funct7 === "b0000101".U && op === "b0110011".U

  val max = funct3 === "b110".U && funct7 === "b0000101".U && op === "b0110011".U
  val maxu = funct3 === "b111".U && funct7 === "b0000101".U && op === "b0110011".U

  val clz = funct3 === "b001".U && funct7 === "b0110000".U && op === "b0010011".U && rs2 === "b00000".U
  val ctz = funct3 === "b001".U && funct7 === "b0110000".U && op === "b0010011".U && rs2 === "b00001".U
  val cpop = funct3 === "b001".U && funct7 === "b0110000".U && op === "b0010011".U && rs2 === "b00010".U

val validInstr = io.valid && (clz || cpop || ctz || maxu || max || minu || min)


 // clz
  leadingZerosCounter.io.input := rs1_data
  val clz_res = leadingZerosCounter.io.result

  // ctz
  val reverser = Module(new reverser(bitWidth))
  reverser.io.input := rs1_data
  leadingZerosCounter.io.input := reverser.io.result
  val ctz_res = leadingZerosCounter.io.result
  
  // cpop
  


  val minMaxU = Module(new minMaxU(bitWidth))
  when(rs1_data(bitWidth-1)===rs2_data(bitWidth-1)){
    //minu, maxu, (min, max iff most significant bits are equal)
    minMaxU.io.in1 := rs1_data
    minMaxU.io.in2 := rs2_data
  }otherwise{
    //max, min
    minMaxU.io.in1 := Cat(0.U,rs1_data(bitWidth-2,0))
    minMaxU.io.in2 := Cat(0.U,rs2_data(bitWidth-2,0))
  }

  val min_res = minMaxU.io.min
  val max_res = minMaxU.io.max


  /* get true instruction result using MuxCase - *plopp* noice! */
  val result = MuxCase(0.U, Seq(
    (clz) -> clz_res,
    (ctz) -> ctz_res,
    //(cpop) -> cpop_res,
    //(minu) -> minu_res,
    (min) -> min_res,
    //(maxu) -> maxu_res,
    (max) -> max_res
  ))
  // write back if valid and not stalled
  io_reg.reg_write_en := validInstr
  io_reg.reg_rd := rd
  io_reg.reg_write_data := result
  io_reg.reg_rs1 := rs1
  
  // increment program counter by + 4
  io_pc.pc_we := validInstr
  io_pc.pc_wdata := io_pc.pc + 4.U
}

class reverser (bitWidth: Int) extends Module {
  val io = IO(new Bundle {
    val input = Input(UInt(bitWidth.W))
    val result = Output(UInt(bitWidth.W))
  })

  if(bitWidth==1){
    io.result := io.input
  }else{
    val rev_rec = Module(new reverser(bitWidth-1))
    rev_rec.io.input := io.input(bitWidth-1,1)
    io.result := Cat(io.input(0),rev_rec.io.result)
  }
}

class minMaxU (bitWidth: Int) extends Module {
  val io = IO(new Bundle {
    val in1 = Input(UInt(bitWidth.W))
    val in2 = Input(UInt(bitWidth.W))
    val min = Output(UInt(bitWidth.W))
    val max = Output(UInt(bitWidth.W))
  })

  //most significant bit determins max (iff unequal of)
  if (bitWidth == 1){
    io.max := Mux(io.in1(0)^io.in2(0) && io.in1(0),io.in1,io.in2)
    io.min := Mux(io.in1(0)^io.in2(0) && io.in1(0),io.in2,io.in1)
  } else {
    when(io.in1(bitWidth-1)^io.in2(bitWidth-1)){
      io.max := Mux(io.in1(bitWidth-1),io.in1,io.in2)
      io.min := Mux(io.in2(bitWidth-1),io.in1,io.in2)
    }otherwise{
    //init recusiveness
     val minMaxU_rec = Module(new minMaxU(bitWidth-1))
     minMaxU_rec.io.in1 := io.in1(bitWidth-2,0)
     minMaxU_rec.io.in2 := io.in2(bitWidth-2,0)

     io.max := minMaxU_rec.io.max
     io.min := minMaxU_rec.io.min
    }
    
  }
}