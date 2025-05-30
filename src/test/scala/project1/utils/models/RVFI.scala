package project1.utils.models

import chisel3._

class RVFI {
  var valid: Bool = false.B
  var order: UInt = 0.U
  var insn: UInt = 0.U
  var trap: Bool = false.B
  var halt: Bool = false.B
  var intr: Bool = false.B
  var mode: UInt = 0.U
  var ixl: UInt = 0.U
  var rs1_addr: UInt = 0.U
  var rs2_addr: UInt = 0.U
  var rs1_rdata: UInt = 0.U
  var rs2_rdata: UInt = 0.U
  var rd_addr: UInt = 0.U
  var rd_wdata: UInt = 0.U
  var pc_rdata: UInt = 0.U
  var pc_wdata: UInt = 0.U
  var mem_addr: UInt = 0.U
  var mem_rmask: UInt = 0.U
  var mem_wmask: UInt = 0.U
  var mem_rdata: UInt = 0.U
  var mem_wdata: UInt = 0.U
}
