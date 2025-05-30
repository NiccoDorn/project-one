package project1.utils

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.util.log2Ceil
import chiseltest._

import scala.language.implicitConversions
import scala.util.Random

object ChiselUtils {

  def setRandomInput(io: Bundle, r: Random): Unit = {
    io.getElements
      .filter(data =>
        DataMirror.specifiedDirectionOf(data) == SpecifiedDirection.Input
      )
      .foreach {
        setRandomInput(_, r)
      }
  }

  def setRandomInput(input: Data, r: Random): Unit = {
    input match {
      case bool: Bool =>
        bool.poke(r.nextBoolean().B)
      case uint: UInt =>
        uint.poke(BigInt(uint.getWidth, r).U)
      case sint: SInt =>
        sint.poke(
          (BigInt(sint.getWidth, r) - (1 << (sint.getWidth - 1))).S
        ) // quick hack
      case _ =>
    }
  }
}
