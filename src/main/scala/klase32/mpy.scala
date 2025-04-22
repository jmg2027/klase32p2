package klase32

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import klase32.include.config._
import klase32.include.param.KLASE32ParamKey
import klase32.include.KLASE32AbstractClass._
import klase32.include.ControlSignal._
// import klase32.interface._


class MPY(implicit p: Parameters) extends CoreModule {

  import MPYControlIE._

  val k = p(KLASE32ParamKey)

  val io = IO(new Bundle {
    val ctrl = Input(MPYControlIE())
    val A = Input(SInt(xLen.W))
    val B = Input(SInt(xLen.W))
    val fusedMul = Input(Bool())
    val prod = Valid(UInt(xLen.W))

    val busy = Output(Bool())
  }
  )

  val rs1Signed = Wire(Bool())
  val rs2Signed = Wire(Bool())
  val mostSignificantWord = Wire(Bool())

  //   Assume control signal does not fall down
  //    val valid = io.ctrl =/= default
  //
  //  rs1Signed := false.B
  //  rs2Signed := false.B
  //  mostSignificantWord := false.B
  //
  //  when(valid) {
  //    when(io.ctrl === MUL) {
  //      rs1Signed := true.B
  //      rs2Signed := true.B
  //      mostSignificantWord := false.B
  //    }.elsewhen(io.ctrl === MULH) {
  //      rs1Signed := false.B
  //      rs2Signed := false.B
  //      mostSignificantWord := true.B
  //    }.elsewhen(io.ctrl === MULHU) {
  //      rs1Signed := true.B
  //      rs2Signed := false.B
  //      mostSignificantWord := true.B
  //    }.otherwise {
  //      rs1Signed := true.B
  //      rs2Signed := false.B
  //      mostSignificantWord := true.B
  //    }
  //
  //    val a = Wire(UInt((xLen + 1).W))
  //    val b = Wire(UInt((xLen + 1).W))
  //    val p = Wire(UInt(((xLen + 1) * 2 + 1).W))
  //
  //    a := Cat((rs1Signed & io.A(31)), io.A.asUInt)
  //    b := Cat((rs2Signed & io.B(31)), io.B.asUInt)
  //    p := a * b
  //    val prod = Mux(mostSignificantWord, p(63, 32).asSInt, p(31, 0).asSInt).asUInt
  //    // What if sequential mul comes in?
  //    io.prod := Pipe(valid, prod, mpyLatency - 1)
  //  }.otherwise {
  //    io.prod.valid := false.B
  //    io.prod.bits := 0.U
  //    val valid = io.ctrl =/= default
  //}

  when(io.ctrl === MUL) {
    rs1Signed := true.B
    rs2Signed := true.B
    mostSignificantWord := false.B
  }.elsewhen(io.ctrl === MULH) {
    rs1Signed := true.B
    rs2Signed := true.B
    mostSignificantWord := true.B
  }.elsewhen(io.ctrl === MULHU) {
    rs1Signed := false.B
    rs2Signed := false.B
    mostSignificantWord := true.B
  }.elsewhen(io.ctrl === MULHSU) {
    rs1Signed := true.B
    rs2Signed := false.B
    mostSignificantWord := true.B
  }.otherwise {
    rs1Signed := false.B
    rs2Signed := false.B
    mostSignificantWord := false.B
  }
  val a = Wire(SInt((xLen + 1).W))
  val b = Wire(SInt((xLen + 1).W))
  val prodRaw = Wire(SInt(((xLen + 1) * 2 + 1).W))

  io.prod.valid := io.ctrl =/= default
  when(io.prod.valid) {
    a := Cat((rs1Signed && io.A(31)), io.A.asUInt).asSInt
    b := Cat((rs2Signed && io.B(31)), io.B.asUInt).asSInt
  }.otherwise {
    a := 0.S
    b := 0.S
  }
  prodRaw := a * b
  val prod = Mux(mostSignificantWord, prodRaw(63, 32).asSInt, prodRaw(31, 0).asSInt).asUInt
  io.prod.bits := prod
  io.busy := false.B
}
