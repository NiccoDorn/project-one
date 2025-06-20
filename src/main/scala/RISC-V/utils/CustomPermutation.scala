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
      newArr((i + amount) % 32) = p(i)
    }
    newArr
  }

  def rotateLeft(p: Array[Int], n: Int): Array[Int] = {
    rotateRight(p, 32 - (n & 31))
  }

  case class OperationInfo(
    name: String,
    opType: String,
    imm: Int,
    func: Array[Int] => Array[Int],
    reverseFunc: Array[Int] => Array[Int]
  )

  case class State(
    perm: List[Int],
    path: List[String],
    lastOpType: String = "",
    lastOpImm: Int = 0
  )

  def findInstructionSequence(targetState: Array[Int], maxDepth: Int, reg: String): Option[List[String]] = {
    val identityPerm = Array.tabulate(32)(i => i)
    if (targetState.sameElements(identityPerm)) return Some(List())

    val ops = {
      val roriOps = (1 until 32).map(i => 
        OperationInfo(s"rori $reg, $reg, $i", "rori", i, p => rotateRight(p, i), p => rotateLeft(p, i))
      )
      val shfliOps = (1 until 16).map(i => 
        OperationInfo(f"shfli $reg, $reg, 0x$i%x", "shfli", i, p => applyShfli(p, i), p => applyUnshfli(p, i))
      )
      val unshfliOps = (1 until 16).map(i => 
        OperationInfo(f"unshfli $reg, $reg, 0x$i%x", "unshfli", i, p => applyUnshfli(p, i), p => applyShfli(p, i))
      )
      val greviOps = (1 until 32).map(i => 
        OperationInfo(f"grevi $reg, $reg, 0x$i%x", "grevi", i, p => applyGrevi(p, i), p => applyGrevi(p, i))
      )
      roriOps ++ shfliOps ++ unshfliOps ++ greviOps
    }

    val fwdQueue = Queue(State(identityPerm.toList, List()))
    val bwdQueue = Queue(State(targetState.toList, List()))

    val fwdVisited = MutableMap[List[Int], List[String]](identityPerm.toList -> List())
    val bwdVisited = MutableMap[List[Int], List[String]](targetState.toList -> List())

    for (depth <- 0 to maxDepth) {
      val fwdLayerSize = fwdQueue.size
      for (_ <- 0 until fwdLayerSize) {
        val curr = fwdQueue.dequeue()

        if (bwdVisited.contains(curr.perm)) {
          val fwdPath = curr.path
          val bwdPath = bwdVisited(curr.perm)
          return Some(fwdPath ++ bwdPath.reverse)
        }

        if (curr.path.length < maxDepth) {
          for (op <- ops) {
            var skip = false
            if (curr.lastOpType.nonEmpty) {
              if (op.opType == "grevi" && curr.lastOpType == "grevi" && op.imm == curr.lastOpImm) skip = true
              else if (op.opType == "shfli" && curr.lastOpType == "unshfli" && op.imm == curr.lastOpImm) skip = true
              else if (op.opType == "unshfli" && curr.lastOpType == "shfli" && op.imm == curr.lastOpImm) skip = true
              else if (op.opType == "rori" && curr.lastOpType == "rori" && (op.imm + curr.lastOpImm) % 32 == 0) skip = true
            }

            if (!skip) {
              val nextPerm = op.func(curr.perm.toArray).toList
              if (!fwdVisited.contains(nextPerm)) {
                val nextPath = curr.path :+ op.name
                fwdVisited(nextPerm) = nextPath
                fwdQueue.enqueue(State(nextPerm, nextPath, op.opType, op.imm))
              }
            }
          }
        }
      }

      val bwdLayerSize = bwdQueue.size
      for (_ <- 0 until bwdLayerSize) {
        val curr = bwdQueue.dequeue()
        
        if (fwdVisited.contains(curr.perm)) {
          val fwdPath = fwdVisited(curr.perm)
          val bwdPath = curr.path
          return Some(fwdPath ++ bwdPath.reverse)
        }

        if (curr.path.length < maxDepth) {
          for (op <- ops) {
            var skip = false
            if (curr.lastOpType.nonEmpty) {
              skip = (op.opType == "grevi" && curr.lastOpType == "grevi" && op.imm == curr.lastOpImm) ||
                (op.opType == "shfli" && curr.lastOpType == "unshfli" && op.imm == curr.lastOpImm) ||
                (op.opType == "unshfli" && curr.lastOpType == "shfli" && op.imm == curr.lastOpImm) ||
                (op.opType == "rori" && curr.lastOpType == "rori" && (op.imm + curr.lastOpImm) % 32 == 0)
            }
            
            if (!skip) {
              val nextPerm = op.reverseFunc(curr.perm.toArray).toList
              if (!bwdVisited.contains(nextPerm)) {
                val nextPath = curr.path :+ op.name
                bwdVisited(nextPerm) = nextPath
                bwdQueue.enqueue(State(nextPerm, nextPath, op.opType, op.imm))
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
  *   The destination register
  * @param rs1
  *   The source register
  * @param perm
  *   A map representing the permutation, mapping destination bit
  *   positions to source bit positions.
  * @return
  *   A list of strings representing the instructions to implement the
  *   permutation e.g. List("grevi x1, x2, 0x01", "grevi x1, x2, 0x02", ...)
  */
  def buildPermutation(rd: Int, rs1: Int, perm: Map[Int, Int]): List[String] = {
    val targetArr = mapToArray(perm)
    val reg = s"x$rd"

    if (isId(targetArr)) {
      return List()
    }

    isRot(targetArr) match {
      case Some(rotAmount) if rotAmount > 0 => return List(s"rori $reg, $reg, $rotAmount")
      case _ =>
    }

    detectSpecialPatterns(perm, reg) match {
      case Some(instr) => return List(instr)
      case None =>
    }

    findInstructionSequence(targetArr, maxDepth = 8, reg) match {
      case Some(result) => List("grevi x1 x1 31") ++ result ++ List("grevi x1 x1 31")
      case None => 
        List()
    }
  }
}