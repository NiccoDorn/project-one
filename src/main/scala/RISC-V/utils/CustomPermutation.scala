package RISCV.utils

import scala.collection.mutable.{Queue, Map => MutableMap}

object PermBuilder {

  def mapToArray(p: Map[Int, Int]): Array[Int] = { // converts for BFS-Queue algo
    val arr = Array.tabulate(32)(i => i)
    p.foreach { case (dest, src) => arr(dest) = src }
    arr
  }
  def arrayToMap(arr: Array[Int]): Map[Int, Int] = {
    arr.zipWithIndex.map { case (src, dest) => dest -> src }.toMap
  }

  // perm && id?
  def isId(p: Array[Int]): Boolean = {
    (0 until 32).forall(i => p(i) == i) // check for identity
  }

  // is rotate?
  def isRot(p: Array[Int]): Option[Int] = {
    val possShifts = (0 until 32).map { i =>
      (i - p(i) + 32) % 32
    }
    val firstShift = possShifts(0)
    if (possShifts.forall(_ == firstShift)) {
      Some(firstShift)
    } else { None }
  }

  // helper for mapping
  def doTransform(curr: Array[Int], transform: String): Array[Int] = {
    val newArr = Array.ofDim[Int](32)

    transform match {
      case s if s.contains("rori") =>
        val amount = extractImm(s)
        for (i <- 0 until 32) {
          val src_pos_before_rori = (i - amount + 32) % 32
          newArr(i) = curr(src_pos_before_rori)
        }

      case s if s.contains("grevi") =>
        val pattern = extractImm(s)
        val grevInverseMapping = Array.tabulate(32)(i => {
          var oldIdx = i
          if ((pattern & (1 << 4)) != 0) oldIdx = if (oldIdx >= 16) oldIdx - 16 else oldIdx + 16
          if ((pattern & (1 << 3)) != 0) oldIdx = if (oldIdx % 16 >= 8) oldIdx - 8 else oldIdx + 8
          if ((pattern & (1 << 2)) != 0) oldIdx = if (oldIdx % 8 >= 4) oldIdx - 4 else oldIdx + 4
          if ((pattern & (1 << 1)) != 0) oldIdx = if (oldIdx % 4 >= 2) oldIdx - 2 else oldIdx + 2
          if ((pattern & (1 << 0)) != 0) oldIdx = if (oldIdx % 2 == 1) oldIdx - 1 else oldIdx + 1
          oldIdx
        })
        for (i <- 0 until 32) {
          newArr(i) = curr(grevInverseMapping(i))
        }

      case s if s.contains("shfli") =>
        val pattern = extractImm(s)
        val shflInverseMapping = Array.tabulate(32)(i => {
          var oldIdx = i
          if ((pattern & (1 << 0)) != 0) oldIdx = oldIdx % 4 match {
              case 0 => (oldIdx / 4) * 4 + 0 // (block_start + 0)
              case 1 => (oldIdx / 4) * 4 + 2 // (block_start + 2) -- original was 2
              case 2 => (oldIdx / 4) * 4 + 1 // (block_start + 1) -- original was 1
              case 3 => (oldIdx / 4) * 4 + 3 // (block_start + 3)
              case _ => oldIdx
            }
          if ((pattern & (1 << 1)) != 0) oldIdx = (oldIdx % 8 / 2) match {
              case 0 => (oldIdx / 8) * 8 + (oldIdx % 2) // from 0/1 to 0/1 within 2-bit sub-block. (oldIdx % 8) will be 0 or 1.
              case 1 => (oldIdx / 8) * 8 + (oldIdx % 2) + 4 // from 2/3 to 6/7. (oldIdx % 8) will be 2 or 3.
              case 2 => (oldIdx / 8) * 8 + (oldIdx % 2) + 2 // from 4/5 to 2/3. (oldIdx % 8) will be 4 or 5.
              case 3 => (oldIdx / 8) * 8 + (oldIdx % 2) + 6 // from 6/7 to 4/5. (oldIdx % 8) will be 6 or 7.
              case _ => oldIdx
            }
          if ((pattern & (1 << 2)) != 0) oldIdx = (oldIdx % 16 / 4) match {
              case 0 => (oldIdx / 16) * 16 + (oldIdx % 4) // from 0-3 to 0-3 within 4-bit sub-block
              case 1 => (oldIdx / 16) * 16 + (oldIdx % 4) + 8 // from 4-7 to 12-15
              case 2 => (oldIdx / 16) * 16 + (oldIdx % 4) + 4 // from 8-11 to 4-7
              case 3 => (oldIdx / 16) * 16 + (oldIdx % 4) + 12 // from 12-15 to 8-11
              case _ => oldIdx
            }
          if ((pattern & (1 << 3)) != 0) oldIdx = (oldIdx % 32 / 8) match {
              case 0 => (oldIdx / 32) * 32 + (oldIdx % 8) // from 0-7 to 0-7 within 8-bit sub-block
              case 1 => (oldIdx / 32) * 32 + (oldIdx % 8) + 16 // from 8-15 to 24-31
              case 2 => (oldIdx / 32) * 32 + (oldIdx % 8) + 8 // from 16-23 to 8-15
              case 3 => (oldIdx / 32) * 32 + (oldIdx % 8) + 24 // from 24-31 to 16-23
              case _ => oldIdx
            }
          oldIdx
        })
        for (i <- 0 until 32) {
          newArr(i) = curr(shflInverseMapping(i))
        }

      case s if s.contains("unshfli") =>
        val pattern = extractImm(s)
        val pShflInverse = Array.tabulate(32)(i => {
          var oldIdx = i
          if ((pattern & (1 << 0)) != 0) oldIdx = oldIdx % 4 match {
              case 0 => (oldIdx / 4) * 4 + 0
              case 1 => (oldIdx / 4) * 4 + 2
              case 2 => (oldIdx / 4) * 4 + 1
              case 3 => (oldIdx / 4) * 4 + 3
              case _ => oldIdx
            }
          if ((pattern & (1 << 1)) != 0) oldIdx = (oldIdx % 8 / 2) match {
              case 0 => (oldIdx / 8) * 8 + (oldIdx % 2)
              case 1 => (oldIdx / 8) * 8 + (oldIdx % 2) + 4
              case 2 => (oldIdx / 8) * 8 + (oldIdx % 2) + 2
              case 3 => (oldIdx / 8) * 8 + (oldIdx % 2) + 6
              case _ => oldIdx
            }
          if ((pattern & (1 << 2)) != 0) oldIdx = (oldIdx % 16 / 4) match {
              case 0 => (oldIdx / 16) * 16 + (oldIdx % 4)
              case 1 => (oldIdx / 16) * 16 + (oldIdx % 4) + 8
              case 2 => (oldIdx / 16) * 16 + (oldIdx % 4) + 4
              case 3 => (oldIdx / 16) * 16 + (oldIdx % 4) + 12
              case _ => oldIdx
            }
          if ((pattern & (1 << 3)) != 0) oldIdx = (oldIdx % 32 / 8) match {
              case 0 => (oldIdx / 32) * 32 + (oldIdx % 8)
              case 1 => (oldIdx / 32) * 32 + (oldIdx % 8) + 16
              case 2 => (oldIdx / 32) * 32 + (oldIdx % 8) + 8
              case 3 => (oldIdx / 32) * 32 + (oldIdx % 8) + 24
              case _ => oldIdx
            }
          oldIdx
        })

        val unshflInverseMapping = Array.ofDim[Int](32)
        for (dest_idx <- 0 until 32) {
          val src_idx_for_dest = pShflInverse(dest_idx)
          unshflInverseMapping(src_idx_for_dest) = dest_idx
        }
        for (i <- 0 until 32) {
          newArr(i) = curr(unshflInverseMapping(i))
        }
      case _ => curr.clone()
    }
    newArr
  }

  // get imm
  def extractImm(instr: String): Int = {
    val parts = instr.split(",")
    if (parts.length >= 3) {
      val immStr = parts(2).trim
      if (immStr.startsWith("0x")) {
        Integer.parseInt(immStr.substring(2), 16)
      } else { immStr.toInt }
    } else 0
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
    val targetArr = mapToArray(perm) // first convert to Arr for BFS

    // id
    val identArr = Array.tabulate(32)(i => i) /* Maybe this is taken for the one failing test? */
    if (isId(targetArr)) {
      return List()
    }

    // easy rotate
    isRot(targetArr) match {
      case Some(rotAmount) =>
        return List(s"rori x$rd, x$rd, $rotAmount")
      case None => // Continue with more complex analysis
    }

    /* BFS Search with Queueing */

    def getInstrBFS(initialState: Array[Int], targetState: Array[Int], maxDepth: Int): Option[List[String]] = {
      val queue = Queue((initialState, List[String]()))
      val vis = MutableMap[Vector[Int], Int]()

      vis.put(initialState.toVector, 0)

      while (queue.nonEmpty) {
        val (currArr, instrList) = queue.dequeue()

        if (currArr.sameElements(targetState)) {
          return Some(instrList)
        }

        if (instrList.length < maxDepth) {
          val roriImms = (1 to 31).toList
          val bitmaskImms = (1 until 32).toList

          val cands =
            roriImms.map(imm => s"rori x$rd, x$rd, $imm") ++
            bitmaskImms.map(imm => s"grevi x$rd, x$rd, 0x${imm.toHexString}") ++
            bitmaskImms.map(imm => s"shfli x$rd, x$rd, 0x${imm.toHexString}") ++
            bitmaskImms.map(imm => s"unshfli x$rd, x$rd, 0x${imm.toHexString}")

          for (cand <- cands) {
            val newCurrArr = doTransform(currArr, cand) // here, apply corresponding transformation case
            val newDepth = instrList.length + 1

            if (!vis.contains(newCurrArr.toVector) || vis(newCurrArr.toVector) > newDepth) {
              vis.put(newCurrArr.toVector, newDepth)
              queue.enqueue((newCurrArr, instrList :+ cand))
            }
          }
        }
      }
      None
    }

    // search depths
    for (depth <- 1 to 4) { // adjust
      getInstrBFS(identArr, targetArr, depth) match {
        case Some(res) => return res
        case None =>
      }
    }
    List()
  }
}