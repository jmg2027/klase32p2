package klase32.functionalunit

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import klase32.include.config._
import klase32.include.param.KLASE32ParamKey
import klase32.include.KLASE32AbstractClass._
import klase32.include.ControlSignal._
import klase32.common.FunctionUnitIO

class MPYReq(implicit p: Parameters) extends CoreBundle {
  val ctrl = MPYControlIE()
  val A = SInt(xLen.W)
  val B = SInt(xLen.W)
  val rd = UInt(regIdWidth.W)
}

class MPYResp(implicit p: Parameters) extends CoreBundle {
  val P = UInt(xLen.W)
  val rd = UInt(regIdWidth.W)
}

class MPY(implicit p: Parameters) extends CoreModule
with FunctionUnitIO[MPYReq, MPYResp] {
  val req = IO(Flipped(Decoupled(new MPYReq)))
  val resp = IO(Decoupled(new MPYResp))

  val core = Module(new MPYCore)

  core.io.ctrl := req.bits.ctrl
  core.io.A := req.bits.A
  core.io.B := req.bits.B

  req.ready := true.B
  resp.valid := req.fire
  resp.bits.P := core.io.out
  resp.bits.rd := req.bits.rd
}

class MPYCore(implicit p: Parameters) extends CoreModule {
  val k = p(KLASE32ParamKey)

  val io = IO(new Bundle {
    val ctrl = Input(MPYControlIE())
    val A = Input(SInt(xLen.W))
    val B = Input(SInt(xLen.W))
    val out = Output(UInt(xLen.W))

    val busy = Output(Bool())
  })

  import MPYControlIE._

  val rs1Signed = Wire(Bool())
  val rs2Signed = Wire(Bool())
  val mostSignificantWord = Wire(Bool())

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
  val prod = Wire(SInt(((xLen + 1) * 2 + 1).W))

  a := Cat((rs1Signed && io.A(31)), io.A.asUInt).asSInt
  b := Cat((rs2Signed && io.B(31)), io.B.asUInt).asSInt

  prod := a * b

  io.out := Mux(mostSignificantWord, prod(63, 32).asSInt, prod(31, 0).asSInt).asUInt
  io.busy := false.B
}
