package klase32

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import klase32.config._
import chisel3.util.log2Ceil
import scala.language.experimental.macros

class QueueWithAccessableEntry[T <: Data](
   override val gen:            T,
   override val entries:        Int,
   override val pipe:           Boolean = false,
   override val flow:           Boolean = false,
   override val useSyncReadMem: Boolean = false,
   override val hasFlush:       Boolean = false
  )(
   implicit compileOptions: chisel3.CompileOptions)
extends Queue[T](gen, entries, pipe, flow, useSyncReadMem, hasFlush) {
  val entry = IO(Output(Vec(entries, genType)))
  for (i <-0 until entries) {
    entry(i) := ram(i)
  }
}

object util {
  def connectBundle[T <: Record](abundle: T, bbundle: T) = {

  }
}
