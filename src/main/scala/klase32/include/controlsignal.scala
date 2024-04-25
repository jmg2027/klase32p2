package klase32

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import klase32.config._
import klase32.param.KlasE32ParamKey
import snitch.enums.SnitchEnum

trait ControlEnum extends SnitchEnum
trait ControlDefaultEnum extends ControlEnum { val default = Value }	// zero default result
trait ControlDefaultEnableEnum extends ControlDefaultEnum {
  val EN = Value
}	// default with enable only

class ALUControlIE {
  // does this work?
  def isSub = this.asInstanceOf[UInt](4)
}

object ALUControlIE extends ControlDefaultEnum {
  val AND = Value	// aluR
  val OR = Value
  val EQ = Value
  val XOR = Value
  val NE = Value
  val SRL = Value
  val SLL = Value
  val LT = Value	// 8
  val LTU = Value
  val GE = Value
  val GEU = Value
  val SLT = Value	// 12
  val SUB = Value
  val SRA = Value
  val SLTU = Value
  val ADD = Value

}


// Should this be onehot?
object DIVControlIE extends ControlDefaultEnum {
  val DIVS = Value
  val DIVU = Value
  val REMS = Value
  val REMU = Value
}

object MPYControlIE extends ControlDefaultEnableEnum

object CtrlControlIE extends ControlDefaultEnum {
  val BR = Value
  val HALT = Value
  val JAL = Value
  val MRET = Value
  val JALR = Value
}

object RegXControlIE extends ControlDefaultEnableEnum
  // Write to GPR with no TCM latency, not from lsu

object LSUControlIE extends ControlDefaultEnum {
  val STB = Value
  val STH = Value
  val STW = Value
  val LDB = Value
  val LDH = Value
  val LDW = Value
}

object W1MuxControlIE extends ControlEnum {
  val DIV = Value	// This can be go out
  val MPYH = Value
  val MPYL = Value
  val ALUR = Value
  val CSR = Value
  val LNK = Value

  override val default = this.DIV
}

object W1deadMuxControlIE extends ControlEnum {
  // To zero register X0
  val LSUR = Value
  val ALUR = Value
  val LNK = Value
  val W1 = Value

  override val default = this.LSUR
}

object ALUAMuxControlIE extends ControlEnum {
  val PC = Value
  val R1 = Value

  override val default = this.PC
}

object ALUBMuxControlIE extends ControlEnum {
  val R2 = Value
  val CI10 = Value	// CIW, [9:4] 4'b0, C.ADDI16SP
  val CIWU = Value	// CIW, [9:2] 2'b0, C.ADDI4SPN
  val IImm = Value
  val SImm = Value
  val IImmU = Value	// unsigned
  val UImm = Value	// signed
  val IImmUSh = Value	// unsigned, lsb 5 bits used only, SLL, SRL, SRA
  val CI = Value	// compressed immediate
  val CI18 = Value	// CI, [17:12] 12'b0, C.LUI
  val CIU = Value	// CI, [5:0], C.SRLI/C.SRAI/C.SLLI
  val CLU = Value	// CI, [6:2] 2'b0, C.SW/C.LW
  val CI8U = Value	// CI, [7:2] 2'b0, C.LWSP
  val CSSU = Value	// CI, [7:2] 2'b0, C.LWSP

  override val default = this.R2
}
object IllegalInstIE extends ControlDefaultEnableEnum

object WFIIE extends ControlDefaultEnableEnum

object JALRIE extends ControlDefaultEnableEnum

object JALIE extends ControlDefaultEnableEnum

object BRIE extends ControlDefaultEnableEnum

object SignedControl extends ControlEnum {
  val unsigned = Value
  val signed = Value

  override val default = this.unsigned
}

object LoadControl extends ControlDefaultEnableEnum

object StoreControl extends ControlDefaultEnableEnum

object OF21MuxIE extends ControlEnum {
  val CJ = Value	// CJ, [11:1], C.J/C.JAL
  val J = Value	// JIMM

  override val default = this.CJ
}

object OF13MuxIE extends ControlDefaultEnum {
  val CB = Value	// CJ, [8:1], C.BEQZ/C.BNEZ
  val B = Value	// BIMM

  override val default = this.CB
}

object TargetMuxIE extends ControlEnum {
  val ALUR = Value	// CJ, [8:1], C.BEQZ/C.BNEZ
  val MEPC = Value	// BIMM

  override val default = this.ALUR
}

object CSRInstMuxIE extends ControlDefaultEnum {
  val RW = Value
  val RS = Value
  val RC = Value
}

object FenceEnableIE extends ControlDefaultEnableEnum

object IcacheFlushIE extends ControlDefaultEnableEnum

object RestoreMstatusIE extends ControlDefaultEnableEnum

object EcallIE extends ControlDefaultEnableEnum

object EbreakIE extends ControlDefaultEnableEnum

object MRetIE extends ControlDefaultEnableEnum

object MpyMDMuxIE extends ControlDefaultEnum {
  // default: MULHU(unsigned a, unsigned b)
  val MULHSU = Value
  val MUL_MULH = Value
}

object OCDSwbreakMuxIE extends ControlDefaultEnableEnum

object W1AddrMuxIE extends ControlEnum {
  // Write port #1 to GPR
  val DIV = Value	// This can be go out
  val RD = Value
  val CLR = Value	// Compressed mode, to x1
  val CSP = Value	// Compressed mode, to x2
  val CRD = Value	// Compressed mode

  override val default = this.DIV
}

object CXW1AddrMuxIE extends ControlDefaultEnum {
  val RD = Value	// C.ADDI4SPN
  val RSD = Value	// C.SRLI, C.SRAI, C.ANDI, C.<OP> (and, or, xor, sub)
}

object HazardIE extends ControlDefaultEnum {
  val RS1 = Value
  val CRSD = Value
  val CRS1 = Value
  val CSP = Value
  val RS2 = Value
  val CRS2_6_2 = Value
  val CRS2_11_7 = Value	// for jalr inst... I think this is mistake
  val CRS2_4_2 = Value
  val CI_11_7 = Value	// Read after div only
  val R = Value
  val RD = Value
  val aluR = Value
  val CADDI4SPN = Value
  val W1ALUOP = Value
  val W1LNKIE = Value
  val W2RD = Value
  val DIV_DIVU = Value
  val DIVW1IE = Value

  // For inst.s use rs1, rs2
  val RS1RS2 = Value
  // For inst.s use rs1, rs2, rd
  val RS1RS2RD = Value
  // For inst.s use rs1, rd
  val RS1RD = Value	// How to deal with this?
}

object LSUControlME extends ControlDefaultEnum {
  val BS = Value
  val HS = Value
  val BU = Value
  val HU = Value
}

object RegXControlME extends ControlDefaultEnableEnum
// Write to GPR with TCM latency from lsu

object HazardME extends ControlDefaultEnum {
  // One hot encoding
  val RD = Value	// for rest loads including C.LWSP
  val CRD = Value	// for C.LW
}
