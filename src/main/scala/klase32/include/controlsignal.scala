package klase32

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import klase32.config._
import klase32.param.KLASE32ParamKey
import snitch.enums.SnitchEnum

trait ControlEnum extends SnitchEnum
trait ControlDefaultEnum extends ControlEnum { val default = Value }	// zero default result
trait ControlDefaultEnableEnum extends ControlDefaultEnum {
  val EN = Value
}	// default with enable only

object ALUControlIE extends ControlDefaultEnum {
  val AND = Value	// aluR 00001
  val OR = Value //       00010
  val SRL = Value //      00011
  val XOR = Value //      00100
  val EQ = Value //       00101
  val NE = Value //       00110
  val SLL = Value //      00111
  val LT = Value	// 8    01000
  val LTU = Value //      01001
  val GE = Value //       01010
  val GEU = Value //      01011
  val SLT = Value	// 12   01100
  val SUB = Value //      01101
  val SRA = Value //      01110
  val SLTU = Value //     01111
  val ADD = Value //      10000

}


object DIVControlIE extends ControlDefaultEnum {
  val DIVS = Value
  val DIVU = Value
  val REMS = Value
  val REMU = Value
}

object MPYControlIE extends ControlDefaultEnableEnum

object FrontendControlIE extends ControlDefaultEnum {
  val BR = Value
  val HALT = Value
  val JAL = Value
  val MRET = Value
  val JALR = Value
}

object W1WritebackIE extends ControlDefaultEnableEnum
// Write to GPR with no TCM latency, not from lsu

object IllegalInstIE extends ControlEnum {
  val normal = Value
  val illegal = Value

  override val default = this.illegal
}

object WFIIE extends ControlDefaultEnableEnum

object SignedControl extends ControlEnum {
  val unsigned = Value
  val signed = Value

  override val default = this.unsigned
}

object LoadControl extends ControlDefaultEnableEnum

object StoreControl extends ControlDefaultEnableEnum

object CSRControl extends ControlDefaultEnum {
  val RW = Value
  val RS = Value
  val RC = Value
}

object FenceEnableIE extends ControlDefaultEnableEnum

object IcacheFlushIE extends ControlDefaultEnableEnum

object EcallIE extends ControlDefaultEnableEnum

object EbreakIE extends ControlDefaultEnableEnum

object MRetIE extends ControlDefaultEnableEnum

object MpyMDMuxIE extends ControlDefaultEnum {
  // default: MULHU(unsigned a, unsigned b)
  val MULHSU = Value
  val MUL_MULH = Value
}

object OCDSwbreakMuxIE extends ControlDefaultEnableEnum
