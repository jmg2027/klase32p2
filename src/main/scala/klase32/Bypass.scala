package klase32

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import klase32.include.config._
import klase32.include.KLASE32AbstractClass._


// Bypass load data to rs1/rs2
class Bypass(implicit p: Parameters) extends CoreModule {
  val io = IO(new Bundle {
    val wb = Input(Vec(writeportNum, Valid(UInt(regIdWidth.W))))
    val rs1 = Input(UInt(regIdWidth.W))
    val rs2 = Input(UInt(regIdWidth.W))

    val bypassRS1 = Output(Bool())
    val bypassRS2 = Output(Bool())
  }
  )

  val bypassRS1 = WireInit(false.B)
  val bypassRS2 = WireInit(false.B)

  for (i <- 0 until writeportNum) {
    when(io.wb(i).valid && io.wb(i).bits =/= 0.U) {
      when(io.wb(i).bits === io.rs1) {
        bypassRS1 := true.B
      }
      when(io.wb(i).bits === io.rs2) {
        bypassRS2 := true.B
      }
    }
  }

  io.bypassRS1 := bypassRS1
  io.bypassRS2 := bypassRS2
}
