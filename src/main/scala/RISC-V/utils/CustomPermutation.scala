package RISCV.utils


object PermBuilder {

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
    *   permutation e.g. List("grev x1, x2, 0x01", "grev x1, x2, 0x02", ...)
    */
  def buildPermutation(rd: Int, rs1: Int, perm: Map[Int, Int]): List[String] = {

    ??? // TODO: implement Task 2.6 here

  }
}
