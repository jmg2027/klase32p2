package klase32.functionalunit

import chisel3._
import chisel3.util._
import klase32.include.KLASE32AbstractClass._
import klase32.include.ControlSignal._
import klase32.include.config._
import klase32.common.FunctionUnitIO

class ALUReq(implicit p: Parameters) extends CoreBundle with HasCoreParameters {
  val ctrl = ALUControlIE()
  val A = UInt(mxLen.W)
  val B = UInt(mxLen.W)
  val rd = UInt(regIdWidth.W)
}

class ALUResp(implicit p: Parameters) extends CoreBundle with HasCoreParameters {
  val F = Bool()
  val R = UInt(mxLen.W)
  val rd = UInt(regIdWidth.W)
}

class ALU(implicit p: Parameters) extends CoreModule with HasCoreParameters
  with FunctionUnitIO[ALUReq, ALUResp] {
  val req = IO(Flipped(Decoupled(new ALUReq)))
  val resp = IO(Decoupled(new ALUResp))

  val aluCore = Module(new ALUCore)

  req.ready := true.B
  aluCore.io.req := req.bits
  resp.valid := req.fire
  resp.bits := aluCore.io.resp
}

class ALUCore(implicit p: Parameters) extends CoreModule with HasCoreParameters {
  val io = IO(new Bundle {
    val req = Input(new ALUReq)
    val resp = Output(new ALUResp)
  })
  import ALUControlIE._
  def isSub = io.req.ctrl.asUInt(3) // GE, GEU, LT, LTU, SLT, SLTU, SUB, SRA
  def isCompareUnsigned = io.req.ctrl.asUInt(0) // GEU, LTU, SLTU
  def isCompareInversed = io.req.ctrl.asUInt(1) // GE, GEU, NE
  def isCompareEqual = !io.req.ctrl.asUInt(3) // EQ, NE

  // Modified Rocket ALU
  // ADD, SUB
  val B_inv = Mux(isSub, ~io.req.B, io.req.B).asUInt
  val adder_result = io.req.A + B_inv + isSub
  val a_xor_b = io.req.A ^ io.req.B

  // LT, LTU
  val lt =
    Mux(io.req.A(mxLen-1) === io.req.B(mxLen-1), adder_result(mxLen-1),
      Mux(isCompareUnsigned, io.req.B(mxLen-1), io.req.A(mxLen-1)))
  // GE, GEU, LT, LTU, SLT, SLTU, EQ, NE
  io.resp.F := Mux(io.req.ctrl === default, 0.U, isCompareInversed ^ Mux(isCompareEqual, a_xor_b === 0.U, lt))

  // SLL, SRL, SRA
  val (shamt, shin_r) = (io.req.B(4,0), io.req.A)
  val shin = Mux(io.req.ctrl === SRL || io.req.ctrl === SRA, shin_r, Reverse(shin_r))
  // isSub filters SRA
  val shout_r = (Cat((isSub) & shin(mxLen-1), shin).asSInt >> shamt)(mxLen-1, 0)
  val shout_l = Reverse(shout_r)
  val shout = Mux(io.req.ctrl === SRL || io.req.ctrl === SRA, shout_r, 0.U) |
    Mux(io.req.ctrl === SLL, shout_l, 0.U)

  // AND, OR, XOR
  // XOR || AND = OR
  val logic = Mux(io.req.ctrl === XOR || io.req.ctrl === OR, a_xor_b, 0.U) |
    Mux(io.req.ctrl === OR || io.req.ctrl === AND, io.req.A & io.req.B, 0.U)

  // SLT, SLTU
  val slt = Mux(io.req.ctrl === SLT || io.req.ctrl === SLTU, lt, 0.U)

  io.resp.R := Mux(io.req.ctrl === ADD || io.req.ctrl === SUB, adder_result, shout | logic | slt)

  // FIXME: rd
  io.resp.rd := io.req.rd
}
