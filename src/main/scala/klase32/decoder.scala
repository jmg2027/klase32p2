package klase32

import chisel3._
import chisel3.util.BitPat
import chisel3.util._
import chisel3.util.experimental.decode.{DecodeField, DecodePattern, DecodeTable}
import chisel3.experimental.BundleLiterals._
import klase32.config._
import klase32.param.KLASE32ParamKey
import klase32.Instructions._
import snitch.enums._


class Decoded(implicit p: Parameters) extends CoreBundle {
  val imm = new Bundle {
    val i = SInt(mxLen.W)
    val u = SInt(mxLen.W)
    val j = SInt(mxLen.W)
    val b = SInt(mxLen.W)
    val s = SInt(mxLen.W)
  }

  val rd = UInt(regIdWidth.W)
  val rs1 = UInt(regIdWidth.W)
  val rs2 = UInt(regIdWidth.W)

  val illegal = IllegalInstIE()
  val operandSelect = new Bundle {
    val a = OperandType()
    val b = OperandType()
  }

  val aluCtrl = ALUControlIE()

  val csrCtrl = CSRControl()

  val rdType = RdType()
//  val useRD = Bool()
  val w1Wb = W1WritebackIE()
//  val w2Wb = RegXControlME()

  val frontendCtrl = FrontendControlIE()

  val lsuCtrl = new LSUControl

  val ecall = EcallIE()
  val ebreak = EbreakIE()
  val mret = MRetIE()

  val fence = FenceEnableIE()
  val flushICache = IcacheFlushIE()
  // val flushTLB = Bool()
  val wfi = WFIIE()

  // val divCtrl = DIVControlIE()
  // val mpyCtrl = MpyMDMuxIE()
}

class DecoderIO(implicit p: Parameters) extends CoreBundle {
  val inst = Input(UInt(mxLen.W))

  val decSig = Output(new Decoded())
}

class Decoder(implicit p: Parameters) extends CoreModule {
  val k = p(KLASE32ParamKey)

  val io = IO(new DecoderIO)

  val d = io.decSig

  val decodeMapping = Seq(
    IllegalField -> d.illegal,

    OpAField -> d.operandSelect.a,
    OpBField -> d.operandSelect.b,

    AluField -> d.aluCtrl,

    CSRControlField -> d.csrCtrl,

    RdField -> d.rdType,
    W1WritebackField -> d.w1Wb,

    CtrlControlIEField -> d.frontendCtrl,

    LsSizeField -> d.lsuCtrl.lsSize,
    StoreField -> d.lsuCtrl.isStore,
    LoadField -> d.lsuCtrl.isLoad,
    SignedField -> d.lsuCtrl.isSigned,

    ECallField -> d.ecall,
    EBreakField -> d.ebreak,
    MRetField -> d.mret,

    FenceField -> d.fence,
    FlushICacheField -> d.flushICache,
    WFIField -> d.wfi,

    // DIVField -> d.divCtrl,
    // MPYMDField -> d.mpyCtrl,
  )

  val instTable =
    RV32IDecode.table ++
      // RV32CDecode.table ++
      RV32MDecode.table

  val decodeTable = new DecodeTable(instTable, decodeMapping.unzip._1)
  val decodedInst = decodeTable.decode(io.inst)
  printf(cf"$decodedInst\n")

  // Set Output
  decodeMapping.map {
    case(a, b) => b := decodedInst(a)
  }

  val instValue = InstParser(io.inst)
  d.rd := instValue.rd
  d.rs1 := instValue.rs1
  d.rs2 := instValue.rs2
  printf(cf"${io.decSig.lsuCtrl.isStore}\n")
  printf(cf"${io.inst(24, 20)}\n")

  d.imm.i := instValue.iimm.asSInt
  d.imm.s := instValue.simm.asSInt
  d.imm.b := instValue.bimm.asSInt
  d.imm.u := instValue.uimm.asSInt
  d.imm.j := instValue.jimm.asSInt

}

case class InstParser(data: UInt) {
  def rd = data(11, 7)
  def rs1 = data(19, 15)
  def rs2 = data(24, 20)

  def iimm = data(31, 20)
  def simm = data(31, 25) ## data(11, 7)
  def bimm = data(31) ## data(7) ## data(30, 25) ## data(11, 8) ## 0.U(1.W)
  def uimm = data(31, 12) ## 0.U(12.W)
  def jimm = data(31) ## data(19, 12) ## data(20) ## data(30, 21) ## 0.U(1.W)
}
