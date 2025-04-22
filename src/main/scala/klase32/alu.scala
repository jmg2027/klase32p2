package klase32

import chisel3._
import chisel3.util._
import klase32.include.KLASE32AbstractClass._
import klase32.include.ControlSignal._
import klase32.include.config._


class ALU(implicit p: Parameters) extends CoreModule with HasCoreParameters {
  val io = IO(new Bundle{
    val ctrl = Input(ALUControlIE())
    val A = Input(UInt(mxLen.W))
    val B = Input(UInt(mxLen.W))
    val F = Output(Bool())
    val R = Output(UInt(mxLen.W))
  }
  )
  import ALUControlIE._

  def isSub = io.ctrl.asUInt(3) // GE, GEU, LT, LTU, SLT, SLTU, SUB, SRA
  def isCompareUnsigned = io.ctrl.asUInt(0) // GEU, LTU, SLTU
  def isCompareInversed = io.ctrl.asUInt(1) // GE, GEU, NE
  def isCompareEqual = !io.ctrl.asUInt(3) // EQ, NE

  // Modified Rocket ALU
  // ADD, SUB
  val B_inv = Mux(isSub, ~io.B, io.B).asUInt
  val adder_result = io.A + B_inv + isSub
  val a_xor_b = io.A ^ io.B

  // LT, LTU
  val lt =
    Mux(io.A(mxLen-1) === io.B(mxLen-1), adder_result(mxLen-1),
      Mux(isCompareUnsigned, io.B(mxLen-1), io.A(mxLen-1)))
  // GE, GEU, LT, LTU, SLT, SLTU, EQ, NE
  io.F := Mux(io.ctrl === default, 0.U, isCompareInversed ^ Mux(isCompareEqual, a_xor_b === 0.U, lt))

  // SLL, SRL, SRA
  val (shamt, shin_r) = (io.B(4,0), io.A)
  val shin = Mux(io.ctrl === SRL || io.ctrl === SRA, shin_r, Reverse(shin_r))
  // isSub filters SRA
  val shout_r = (Cat((isSub) & shin(mxLen-1), shin).asSInt >> shamt)(mxLen-1, 0)
  val shout_l = Reverse(shout_r)
  val shout = Mux(io.ctrl === SRL || io.ctrl === SRA, shout_r, 0.U) |
    Mux(io.ctrl === SLL, shout_l, 0.U)

  // AND, OR, XOR
  // XOR || AND = OR
  val logic = Mux(io.ctrl === XOR || io.ctrl === OR, a_xor_b, 0.U) |
    Mux(io.ctrl === OR || io.ctrl === AND, io.A & io.B, 0.U)

  // SLT, SLTU
  val slt = Mux(io.ctrl === SLT || io.ctrl === SLTU, lt, 0.U)

  io.R := Mux(io.ctrl === ADD || io.ctrl === SUB, adder_result, shout | logic | slt)
}
