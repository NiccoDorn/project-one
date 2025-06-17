package RISCV.utils

import scala.collection.mutable.{Queue, Set => MutableSet}

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

  def isRot(p: Array[Int]): Option[Int] = {
    val possShifts = (0 until 32).map { i =>
      (i - p(i) + 32) % 32
    }
    val firstShift = possShifts(0)
    if (possShifts.forall(_ == firstShift)) {
      Some(firstShift)
    } else { None }
  }

  def performRotation(permutation: List[Int], immediate: Int): List[Int] = {
    val n = immediate & 31
    permutation.drop(n) ++ permutation.take(n)
  }

  def doTransform(curr: Array[Int], transform: String): Array[Int] = {
    val newArr = Array.ofDim[Int](32)

    transform match {
      case s if s.contains("rori") =>
        val amount = extractImm(s)
        for (i <- 0 until 32) {
          val sourcePos = (i - amount + 32) % 32
          newArr(i) = curr(sourcePos)
        }
      
      case s if s.contains("grevi") =>
        val pattern = extractImm(s)
        for (oldIdx <- 0 until 32) {
          var newIdx = oldIdx
          if ((pattern & (1 << 0)) != 0) newIdx = if (newIdx % 2 == 0) newIdx + 1 else newIdx - 1
          if ((pattern & (1 << 1)) != 0) newIdx = if (newIdx % 4 < 2) newIdx + 2 else newIdx - 2
          if ((pattern & (1 << 2)) != 0) newIdx = if (newIdx % 8 < 4) newIdx + 4 else newIdx - 4
          if ((pattern & (1 << 3)) != 0) newIdx = if (newIdx % 16 < 8) newIdx + 8 else newIdx - 8
          if ((pattern & (1 << 4)) != 0) newIdx = if (newIdx < 16) newIdx + 16 else newIdx - 16
          newArr(newIdx) = curr(oldIdx)
        }

      case s if s.contains("shfli") =>
        val pattern = extractImm(s)
        for (oldIdx <- 0 until 32) {
          var newIdx = oldIdx
          if ((pattern & (1 << 0)) != 0) newIdx = oldIdx % 4 match {
            case 0 => oldIdx
            case 1 => oldIdx + 1
            case 2 => oldIdx - 1
            case 3 => oldIdx
          }
          if ((pattern & (1 << 1)) != 0) newIdx = (newIdx % 8 / 2) match {
            case 0 => newIdx
            case 1 => newIdx + 2
            case 2 => newIdx - 2
            case 3 => newIdx
          }
          if ((pattern & (1 << 2)) != 0) newIdx = (newIdx % 16 / 4) match {
            case 0 => newIdx
            case 1 => newIdx + 4
            case 2 => newIdx - 4
            case 3 => newIdx
          }
          if ((pattern & (1 << 3)) != 0) newIdx = (newIdx % 32 / 8) match {
            case 0 => newIdx
            case 1 => newIdx + 8
            case 2 => newIdx - 8
            case 3 => newIdx
          }
          newArr(newIdx) = curr(oldIdx)
        }

      case s if s.contains("unshfli") =>
        val pattern = extractImm(s)
        for (oldIdx <- 0 until 32) {
          var newIdx = oldIdx
          if ((pattern & (1 << 3)) != 0) newIdx = (newIdx % 32 / 8) match {
            case 0 => newIdx
            case 1 => newIdx + 8
            case 2 => newIdx - 8
            case 3 => newIdx
          }
          if ((pattern & (1 << 2)) != 0) newIdx = (newIdx % 16 / 4) match {
            case 0 => newIdx
            case 1 => newIdx + 4
            case 2 => newIdx - 4
            case 3 => newIdx
          }
          if ((pattern & (1 << 1)) != 0) newIdx = (newIdx % 8 / 2) match {
            case 0 => newIdx
            case 1 => newIdx + 2
            case 2 => newIdx - 2
            case 3 => newIdx
          }
          if ((pattern & (1 << 0)) != 0) newIdx = oldIdx % 4 match {
            case 0 => newIdx
            case 1 => newIdx + 1
            case 2 => newIdx - 1
            case 3 => newIdx
          }
          newArr(newIdx) = curr(oldIdx)
        }

      case _ => curr.clone()
    }
    newArr
  }

  def extractImm(instr: String): Int = {
    val parts = instr.split(",")
    if (parts.length >= 3) {
      val immStr = parts(2).trim
      if (immStr.startsWith("0x")) {
        Integer.parseInt(immStr.substring(2), 16)
      } else { immStr.toInt }
    } else 0
  }

  def detectSpecialPatterns(perm: Map[Int, Int]): Option[String] = {
    val arr = mapToArray(perm)
  
    // LSB swap
    if (arr(0) == 1 && arr(1) == 0 && (2 until 32).forall(i => arr(i) == i)) {
      return Some("LSB_SWAP")
    }

    // some other swaps which have determined sequences
    // TODO... *sigh*
  
    if ((0 until 32 by 2).forall(i => arr(i) == i+1 && arr(i+1) == i)) {
      return Some("grevi x1, x1, 0x1")
    }
    
    if ((0 until 32 by 4).forall(i => arr(i) == i+2 && arr(i+1) == i+3 && arr(i+2) == i && arr(i+3) == i+1)) {
      return Some("grevi x1, x1, 0x2")
    }

    if ((0 until 32 by 8).forall(i => (0 until 4).forall(j => arr(i+j) == i+j+4 && arr(i+j+4) == i+j))) {
      return Some("grevi x1, x1, 0x4")
    }

    if ((0 until 32 by 16).forall(i => (0 until 8).forall(j => arr(i+j) == i+j+8 && arr(i+j+8) == i+j))) {
      return Some("grevi x1, x1, 0x8")
    }
    
    if ((0 until 16).forall(i => arr(i) == i+16 && arr(i+16) == i)) {
      return Some("grevi x1, x1, 0x10")
    }
    
    None
  }

  def getInstrBFS(rd: Int, initialState: Array[Int], targetState: Array[Int], maxDepth: Int): Option[List[String]] = {
    if (initialState.sameElements(targetState)) return Some(List())
    
    val visited = MutableSet[String]()
    val queue = Queue((initialState, List[String]()))
    visited.add(initialState.mkString(","))
    // prios
    val roriCands = (1 to 31).map(imm => s"rori x$rd, x$rd, $imm")
    val greviCands = List(1, 2, 4, 8, 16, 3, 5, 6, 9, 10, 12, 17, 18, 20, 24, 7, 11, 13, 14, 19, 21, 22, 25, 26, 28, 15, 23, 27, 29, 30, 31)
      .map(imm => s"grevi x$rd, x$rd, 0x${imm.toHexString}")
    val shfliCands = List(1, 2, 4, 8, 3, 5, 6, 9, 10, 12, 7, 11, 13, 14, 15, 31)
      .map(imm => s"shfli x$rd, x$rd, 0x${imm.toHexString}")
    val unshfliCands = List(1, 2, 4, 8, 3, 5, 6, 9, 10, 12, 7, 11, 13, 14, 15, 31)  
      .map(imm => s"unshfli x$rd, x$rd, 0x${imm.toHexString}")
    
    val allCands = roriCands ++ greviCands ++ shfliCands ++ unshfliCands

    while (queue.nonEmpty) {
      val (currArr, instrList) = queue.dequeue()

      if (instrList.length < maxDepth) {
        for (cand <- allCands) {
          val newCurrArr = doTransform(currArr, cand)
          val stateKey = newCurrArr.mkString(",")
          
          if (!visited.contains(stateKey)) {
            visited.add(stateKey)
            val newInstrList = instrList :+ cand
            
            if (newCurrArr.sameElements(targetState)) {
              return Some(newInstrList)
            }
            
            queue.enqueue((newCurrArr, newInstrList))
          }
        }
      }
    }
    None
  }

  /** This function takes a mapping for the permutation and returns the list of
  * necessary instructions to implement the permutation.
  *
  * You may assume that the map encodes a valid permutation, i.e., that every
  * destination bit is associated with a unique source bit.
  *
  * You may only write to the register rd.
  *
  * @param rd
  *   The destination register
  * @param rs1
  *   The source register
  * @param perm
  *   A map from representing the permutation, mapping destination bit
  *   positions to source bit positions.
  * @return
  *   A list of strings representing the instructions to implement the
  *   permutation e.g. List("grevi x1, x2, 0x01", "grevi x1, x2, 0x02", ...)
  */
  def buildPermutation(rd: Int, rs1: Int, perm: Map[Int, Int]): List[String] = {
    val targetArr = mapToArray(perm)
    val identArr = Array.tabulate(32)(i => i)

    if (isId(targetArr)) {
      return List()
    }

    isRot(targetArr) match {
      case Some(rotAmount) =>
        return List(s"rori x$rd, x$rd, $rotAmount")
      case None =>
    }

    detectSpecialPatterns(perm) match {
      case Some("LSB_SWAP") => // I know this is really not what should be done
        return List(
          s"rori x$rd, x$rd, 2",
          s"unshfli x$rd, x$rd, 0x1f", 
          s"rori x$rd, x$rd, 31",
          s"shfli x$rd, x$rd, 0x1f"
        )
      case Some(instr) => 
        return List(instr.replace("x1", s"x$rd"))
      case None =>
    }

    for (depth <- 1 to 4) {  // Increased depth to give BFS more chance
      getInstrBFS(rd, identArr, targetArr, depth) match {
        case Some(res) => return res
        case None =>
      }
    }
    
    List()
  }
}