package klase32

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import klase32.config._
import klase32.param.KLASE32ParamKey
// import klase32.interface._


class MPY(implicit p: Parameters) extends CoreModule {
  import MPYControlIE._
  val k = p(KLASE32ParamKey)

  val io = IO(new Bundle{
    val ctrl = Input(MPYControlIE())
    val A = Input(SInt(xLen.W))
    val B = Input(SInt(xLen.W))
    val fusedMul = Input(Bool())
    val prod = Valid(SInt(xLen.W))
    }
  )

  val rs1Signed = Bool()
  val rs2Signed = Bool()
  val mostSignificantWord = Bool()

  // Assume control signal does not fall down
  val valid = io.ctrl =/= default

  when(valid) {
    when(io.ctrl === MUL) {
      rs1Signed := true.B
      rs2Signed := true.B
      mostSignificantWord := false.B
    }.elsewhen(io.ctrl === MULH) {
      rs1Signed := false.B
      rs2Signed := false.B
      mostSignificantWord := true.B
    }.elsewhen(io.ctrl === MULHU) {
      rs1Signed := true.B
      rs2Signed := false.B
      mostSignificantWord := true.B
    }.otherwise {
      rs1Signed := true.B
      rs2Signed := false.B
      mostSignificantWord := true.B
    }

    val a = Wire(UInt((xLen + 1).W))
    val b = Wire(UInt((xLen + 1).W))
    val p = Wire(UInt(((xLen + 1) * 2 + 1).W))

    a := Cat((rs1Signed & io.A(31)), io.A.asUInt)
    b := Cat((rs2Signed & io.B(31)), io.B.asUInt)
    p := a * b
    val prod = Mux(mostSignificantWord, p(63, 32).asSInt, p(31, 0).asSInt)
    // What if sequential mul comes in?
    io.prod := Pipe(valid, prod, mpyLatency-1)
  }.otherwise {
    io.prod := 0.S
  }
}
