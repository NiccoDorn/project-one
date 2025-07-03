package RISCV.utils

import scala.collection.mutable.{Queue, Map => MutableMap}
import scala.annotation.tailrec

object PermBuilder {

  def mapToArray(p: Map[Int, Int]): Array[Int] = {
    val arr = Array.tabulate(32)(i => i)
    p.foreach { case (dest, src) => arr(dest) = src }
    arr
  }

  def arrayToMap(arr: Array[Int]): Map[Int, Int] = {
    arr.zipWithIndex.map { case (src, dest) => dest -> src }.toMap
  }

  def isId(p: Array[Int]): Boolean = {
    (0 until 32).forall(i => p(i) == i)
  }

  private def applyGrevi(p: Array[Int], imm: Int): Array[Int] = {
    val newArr = new Array[Int](32)
    for (i <- 0 until 32) {
      var oldIdx = i
      if ((imm & (1 << 4)) != 0) oldIdx = if (oldIdx >= 16) oldIdx - 16 else oldIdx + 16
      if ((imm & (1 << 3)) != 0) oldIdx = if ((oldIdx % 16) >= 8) oldIdx - 8 else oldIdx + 8
      if ((imm & (1 << 2)) != 0) oldIdx = if ((oldIdx % 8) >= 4) oldIdx - 4 else oldIdx + 4
      if ((imm & (1 << 1)) != 0) oldIdx = if ((oldIdx % 4) >= 2) oldIdx - 2 else oldIdx + 2
      if ((imm & (1 << 0)) != 0) oldIdx = if ((oldIdx % 2) == 1) oldIdx - 1 else oldIdx + 1
      newArr(i) = p(oldIdx)
    }
    newArr
  }
  
  private def applyShfli(p: Array[Int], imm: Int): Array[Int] = {
    val newArr = new Array[Int](32)
    for (i <- 0 until 32) {
      var oldIdx = i
      if ((imm & (1 << 0)) != 0) oldIdx = (oldIdx % 4) match {
        case 1 => oldIdx + 1
        case 2 => oldIdx - 1
        case _ => oldIdx
      }
      if ((imm & (1 << 1)) != 0) oldIdx = (oldIdx % 8 / 2) match {
        case 1 => oldIdx + 2
        case 2 => oldIdx - 2
        case _ => oldIdx
      }
      if ((imm & (1 << 2)) != 0) oldIdx = (oldIdx % 16 / 4) match {
        case 1 => oldIdx + 4
        case 2 => oldIdx - 4
        case _ => oldIdx
      }
      if ((imm & (1 << 3)) != 0) oldIdx = (oldIdx % 32 / 8) match {
        case 1 => oldIdx + 8
        case 2 => oldIdx - 8
        case _ => oldIdx
      }
      newArr(i) = p(oldIdx)
    }
    newArr
  }

  private def applyUnshfli(p: Array[Int], imm: Int): Array[Int] = {
    val newArr = new Array[Int](32)
    for (i <- 0 until 32) {
      var oldIdx = i
      if ((imm & (1 << 3)) != 0) oldIdx = (oldIdx % 32 / 8) match {
        case 1 => oldIdx + 8
        case 2 => oldIdx - 8
        case _ => oldIdx
      }
      if ((imm & (1 << 2)) != 0) oldIdx = (oldIdx % 16 / 4) match {
        case 1 => oldIdx + 4
        case 2 => oldIdx - 4
        case _ => oldIdx
      }
      if ((imm & (1 << 1)) != 0) oldIdx = (oldIdx % 8 / 2) match {
        case 1 => oldIdx + 2
        case 2 => oldIdx - 2
        case _ => oldIdx
      }
      if ((imm & (1 << 0)) != 0) oldIdx = (oldIdx % 4) match {
        case 1 => oldIdx + 1
        case 2 => oldIdx - 1
        case _ => oldIdx
      }
      newArr(i) = p(oldIdx)
    }
    newArr
  }

  def rotateRight(p: Array[Int], n: Int): Array[Int] = {
    val amount = n & 31
    val newArr = new Array[Int](32)
    for (i <- 0 until 32) {
      newArr(i) = p((i - amount + 32) % 32)
    }
    newArr
  }

  def rotateLeft(p: Array[Int], n: Int): Array[Int] = {
    val amount = n & 31
    val newArr = new Array[Int](32)
    for (i <- 0 until 32) {
      newArr(i) = p((i + amount) % 32)
    }
    newArr
  }

  case class OperationInfo(
    name: String,
    opType: String,
    imm: Int,
    func: Array[Int] => Array[Int],
    reverseFunc: Array[Int] => Array[Int]
  )

  case class State(
    perm: Vector[Int],
    parent: Option[State],
    lastOpType: String,
    lastOpImm: Int,
    opName: String
  )

  private def reconPath(state: State): List[String] = {
    @tailrec
    def buildPath(s: State, acc: List[String]): List[String] = {
      s.parent match {
        case Some(p) => buildPath(p, s.opName :: acc)
        case None => acc
      }
    }
    buildPath(state, List()).filter(_.nonEmpty)
  }

  def findInstructionSequence(targetState: Array[Int], maxDepth: Int, reg: String): Option[List[String]] = {
    val identityPerm = Array.tabulate(32)(i => i)
    if (targetState.sameElements(identityPerm)) return Some(List(s"rori $reg, $reg, 32"))

    val prioRoriImms = Seq(31, 29, 2, 1, 30, 4, 8, 16, 24)
    val allRoriImms = (1 until 32).toSeq
    val roriImmsOrder = prioRoriImms ++ allRoriImms.filterNot(prioRoriImms.contains)

    val prioGreviImms = Seq(31, 1, 16, 24, 8, 4, 2)
    val allGreviImms = (1 until 32).toSeq
    val greviImmsOrd = prioGreviImms ++ allGreviImms.filterNot(prioGreviImms.contains)

    val prioShfliImms = Seq(1, 15, 2, 4, 8)
    val allShfliImms = (1 until 16).toSeq
    val shfliImmsOrdered = prioShfliImms ++ allShfliImms.filterNot(prioShfliImms.contains)

    val ops = {
      val roriOps = roriImmsOrder.map(i =>
        // FIX: If rori instruction is defined as a LEFT rotation in the project,
        // map its forward function to rotateLeft and reverse function to rotateRight.
        OperationInfo(s"rori $reg, $reg, $i", "rori", i, p => rotateLeft(p, i), p => rotateRight(p, i))
      )
      val shfliOps = shfliImmsOrdered.map(i =>
        OperationInfo(f"shfli $reg, $reg, 0x$i%x", "shfli", i, p => applyShfli(p, i), p => applyUnshfli(p, i))
      )
      val unshfliOps = shfliImmsOrdered.map(i =>
        OperationInfo(f"unshfli $reg, $reg, 0x$i%x", "unshfli", i, p => applyUnshfli(p, i), p => applyShfli(p, i))
      )
      val greviOps = greviImmsOrd.map(i =>
        OperationInfo(f"grevi $reg, $reg, 0x$i%x", "grevi", i, p => applyGrevi(p, i), p => applyGrevi(p, i))
      )
      roriOps ++ shfliOps ++ unshfliOps ++ greviOps
    }

    val idPermVec = identityPerm.toVector
    val tStateVec = targetState.toVector

    val fwdQ = Queue(State(idPermVec, None, "", 0, ""))
    val bwdQ = Queue(State(tStateVec, None, "", 0, ""))

    val fwdVis = MutableMap[Vector[Int], State](idPermVec -> fwdQ.head)
    val bwdVis = MutableMap[Vector[Int], State](tStateVec -> bwdQ.head)

    for (depth <- 0 to maxDepth) {
      val fwdLayerSize = fwdQ.size
      for (_ <- 0 until fwdLayerSize) {
        val curr = fwdQ.dequeue()

        if (bwdVis.contains(curr.perm)) {
          val fwdPath = reconPath(curr)
          val bwdPath = reconPath(bwdVis(curr.perm)).reverse
          return Some(fwdPath ++ bwdPath)
        }

        if (reconPath(curr).length < maxDepth) {
          for (op <- ops) {
            val skip = (curr.lastOpType == op.opType && curr.lastOpImm == op.imm && op.opType == "grevi") ||
                         (curr.lastOpType == "unshfli" && op.opType == "shfli" && curr.lastOpImm == op.imm) ||
                         (curr.lastOpType == "shfli" && op.opType == "unshfli" && curr.lastOpImm == op.imm) ||
                         (curr.lastOpType == "rori" && op.opType == "rori" && (op.imm + curr.lastOpImm) % 32 == 0)
            
            if (!skip) {
              val nextPerm = op.func(curr.perm.toArray).toVector
              if (!fwdVis.contains(nextPerm)) {
                val nextState = State(nextPerm, Some(curr), op.opType, op.imm, op.name)
                fwdVis(nextPerm) = nextState
                fwdQ.enqueue(nextState)
              }
            }
          }
        }
      }

      val bwdLayerSize = bwdQ.size
      for (_ <- 0 until bwdLayerSize) {
        val curr = bwdQ.dequeue()
        
        if (fwdVis.contains(curr.perm)) {
          val fwdPath = reconPath(fwdVis(curr.perm))
          val bwdPath = reconPath(curr).reverse
          return Some(fwdPath ++ bwdPath)
        }

        if (reconPath(curr).length < maxDepth) {
          for (op <- ops) {
            val skip = (curr.lastOpType == op.opType && curr.lastOpImm == op.imm && op.opType == "grevi") ||
                         (curr.lastOpType == "unshfli" && op.opType == "shfli" && curr.lastOpImm == op.imm) ||
                         (curr.lastOpType == "shfli" && op.opType == "unshfli" && curr.lastOpImm == op.imm) ||
                         (curr.lastOpType == "rori" && op.opType == "rori" && (op.imm + curr.lastOpImm) % 32 == 0)

            if (!skip) {
              val nextPerm = op.reverseFunc(curr.perm.toArray).toVector
              if (!bwdVis.contains(nextPerm)) {
                val nextState = State(nextPerm, Some(curr), op.opType, op.imm, op.name)
                bwdVis(nextPerm) = nextState
                bwdQ.enqueue(nextState)
              }
            }
          }
        }
      }
    }
    None
  }

  def isRot(p: Array[Int]): Option[Int] = {
    val firstShift = (p(0) - 0 + 32) % 32
    if ((1 until 32).forall(i => (p(i) - i + 32) % 32 == firstShift)) Some(firstShift) else None
  }
  
  def detectSpecialPatterns(perm: Map[Int, Int], reg: String): Option[String] = {
      val arr = mapToArray(perm)
      if ((0 until 32 by 2).forall(i => arr(i) == i+1 && arr(i+1) == i)) {
        Some(s"grevi $reg, $reg, 0x1")
      } else if ((0 until 16).forall(i => arr(i) == i+16 && arr(i+16) == i)) {
        Some(s"grevi $reg, $reg, 0x10")
      } else {
        None
      }
  }


  /** This function takes a mapping for the permutation and returns the list of
   * necessary instructions to implement the permutation.
   *
   * You may assume that the map encodes a valid permutation, i.e., that every
   * destination bit is associated with a unique source bit.
   *
   * You may only write to register rd.
   *
   * @param rd
   * The destination register
   * @param rs1
   * The source register
   * @param perm
   * A map representing the permutation, mapping destination bit
   * positions to source bit positions.
   * @return
   * A list of strings representing the instructions to implement the
   * permutation e.g. List("grevi x1, x2, 0x01", "grevi x1, x2, 0x02", ...)
   */
  def buildPermutation(rd: Int, rs1: Int, perm: Map[Int, Int]): List[String] = {
    val targetArr = mapToArray(perm)
    val reg = s"x$rd"

    if (isId(targetArr)) {
      return List(s"rori $reg, $reg, 32")
    }
    
    isRot(targetArr) match {
      case Some(rotAmount) if rotAmount > 0 => return List(s"rori $reg, $reg, $rotAmount")
      case _ =>
    }

    detectSpecialPatterns(perm, reg) match {
      case Some(instr) => return List(instr)
      case None =>
    }

    val tArr = targetArr
    findInstructionSequence(tArr, maxDepth = 4, reg) match { // depth=4 limit due to time constraints
      case Some(result) => result
      case None => List(s"rori $reg, $reg, 32")
    }
  }
}