package klase32.functionalunit

import chisel3._
import chisel3.util._
import klase32.include.config._
import klase32.include.param.KLASE32ParamKey
import klase32.include.KLASE32AbstractClass._

// RAW Scoreboard for GPR
class Scoreboard(implicit p: Parameters) extends CoreBundle {
  val io = IO(new Bundle{
    val rs1 = Input(UInt(regIdWidth.W))
    val rs2 = Input(UInt(regIdWidth.W))
    val set = Input(Valid(UInt(regIdWidth.W)))
    val clr = Input(Valid(UInt(regIdWidth.W)))

    val stall = Output(Bool())
  })

  private val busy = RegInit(VecInit(Seq.fill(regNum)(false.B)))
  when (io.set.valid && io.set.bits =/= 0.U) {busy(io.set.bits) := true.B}
  when (io.clr.valid && io.clr.bits =/= 0.U) {busy(io.clr.bits) := false.B}

  def stall: Bool = busy(io.rs1) || busy(io.rs2)
  io.stall := stall
}
