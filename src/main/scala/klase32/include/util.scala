package klase32.include

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.requireIsChiselType
import klase32.include.config._
import chisel3.util.log2Ceil

import scala.language.experimental.macros

class QueueWithAccessableEntryWithValid[T <: Data](
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
  val valid = RegInit(VecInit(Seq.tabulate(entries){i => false.B}))

  for (i <- 0 until entries) {
    when(valid(i)) {
      entry(i) := ram(i)
    }.otherwise {
      entry(i) := 0.U.asTypeOf(genType)
    }
  }
  when(do_enq) {
    valid(enq_ptr.value) := true.B
  }
  when(do_deq) {
    valid(deq_ptr.value) := false.B
  }
  when(flush) {
    for (i <- 0 until entries) valid(i) := false.B
  }
}

object util {
  def connectBundle[T <: Record](abundle: T, bbundle: T) = {

  }

  class LeadingOneDetector(val width: Int) extends Module {
    val io = IO(new Bundle {
      val in = Input(UInt(width.W))
      val out = Output(UInt(log2Ceil(width).W))
    })

    def leadingOneDetect(in: UInt): UInt = {
      val n = in.getWidth
      if (n == 2) {
        Mux(in(1), 1.U, 0.U)
      } else {
        val half = n / 2
        val upper = in(n - 1, half)
        val lower = in(half - 1, 0)
        Mux(upper.orR, Cat(1.U(1.W), leadingOneDetect(upper)), Cat(0.U(1.W), leadingOneDetect(lower)))
      }
    }

    io.out := leadingOneDetect(io.in)
  }
}
