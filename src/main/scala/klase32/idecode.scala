package klase32

import chisel3._
import chisel3.util._

import chisel3.util.experimental.decode.{DecodeField, DecodePattern, BoolDecodeField}
import snitch.enums._


/**
  * Definitions of Decoder Base classes
  */

class InstProperty(val v: Data) {
  var chain: List[InstProperty] = List(this)
  var isCompose = false

  def ++(prop: InstProperty) = {
    this.chain = this.chain ++ prop.chain
    this
  }

  def this(prop: InstProperty) = {
    this(0.U)
    this ++ prop
    this.isCompose = true
  }

  def toList = chain.filter(! _.isCompose)
}

case class InstPattern(val pat: BitPat, val prop: InstProperty) extends DecodePattern {
  def this(code: String, prop: InstProperty) = this(BitPat(code), prop)
  def bitPat: BitPat = pat
}

abstract class InstEnumField(e: SnitchEnum) extends DecodeField[InstPattern, EnumType] {
  // override def default = BitPat(e.default.litOption.get.U(e.getWidth.W)) // Every default value is 0
  def typeChecker: (InstProperty => Boolean)

  override def chiselType: EnumType = e()
  override def genTable(p: InstPattern) = {
    val ret = p.prop.toList
      .filter(typeChecker)
      .map(_.v.asInstanceOf[EnumType])
      .headOption
      .getOrElse(e.default)
    BitPat(ret.litOption.get.U(e.getWidth.W))
  }
}

abstract class InstBoolField extends BoolDecodeField[InstPattern] {
  // override def default = BitPat(_default) // Every default value is 0
  def typeChecker: (InstProperty => Boolean)

  override def genTable(p: InstPattern) = {
    val ret = p.prop.toList
      .filter(typeChecker)
      .map(_.v.asInstanceOf[UInt])
      .headOption
      .getOrElse(false.B)
    BitPat(ret)
  }
}

/**
  * Definitions of Decoder Properties and Fields
  */

case class IllegalProperty(op: IllegalInstIE.Type) extends InstProperty(op)
object IllegalField extends InstEnumField(IllegalInstIE) {
  override def name = "illegal"
  override def typeChecker = _.isInstanceOf[IllegalProperty]
}

case class OpAProperty(op: OperandType.Type) extends InstProperty(op)
object OpAField extends InstEnumField(OperandType) {
  override def name = "op a"
  override def typeChecker = _.isInstanceOf[OpAProperty]
}

case class OpBProperty(op: OperandType.Type) extends InstProperty(op)
object OpBField extends InstEnumField(OperandType) {
  override def name = "op b"
  override def typeChecker = _.isInstanceOf[OpBProperty]
}

case class AluProperty(op: ALUControlIE.Type) extends InstProperty(op)
object AluField extends InstEnumField(ALUControlIE) {
  override def name = "alu"
  override def typeChecker = _.isInstanceOf[AluProperty]
}

case class CSRControlProperty(op: CSRControl.Type) extends InstProperty(op)
object CSRControlField extends InstEnumField(CSRControl) {
  override def name = "csr instruction"
  override def typeChecker = _.isInstanceOf[CSRControlProperty]
}

case class RdProperty(op: RdType.Type) extends InstProperty(op)
object RdField extends InstEnumField(RdType) {
  override def name = "rd"
  override def typeChecker = _.isInstanceOf[RdProperty]
}

case class WriteRdProperty(op: Bool) extends InstProperty(op)
object WriteRdField extends InstBoolField {
  override def name = "write rd"
  override def typeChecker = _.isInstanceOf[WriteRdProperty]
}

case class W1WritebackProperty(op: W1WritebackIE.Type) extends InstProperty(op)
object W1WritebackField extends InstEnumField(W1WritebackIE) {
  override def name = "w1 writeback"
  override def typeChecker = _.isInstanceOf[W1WritebackProperty]
}

case class UseRdProperty(op: Bool) extends InstProperty(op)
object UseRdField extends InstBoolField {
  override def name = "use rd"
  override def typeChecker = _.isInstanceOf[UseRdProperty]
}

case class NextPcProperty(op: PcType.Type) extends InstProperty(op)
object NextPcField extends InstEnumField(PcType) {
  override def name = "next pc"
  override def typeChecker = _.isInstanceOf[NextPcProperty]
}

case class CtrlControlIEProperty(op: FrontendControlIE.Type) extends InstProperty(op)
object CtrlControlIEField extends InstEnumField(FrontendControlIE) {
  override def name = "decoded jumps"
  override def typeChecker = _.isInstanceOf[CtrlControlIEProperty]
}

case class LsSizeProperty(op: DataSize.Type) extends InstProperty(op)
object LsSizeField extends InstEnumField(DataSize) {
  override def name = "ls size"
  override def typeChecker = _.isInstanceOf[LsSizeProperty]
}

case class StoreProperty(op: StoreControl.Type) extends InstProperty(op)
object StoreField extends InstEnumField(StoreControl) {
  override def name = "store"
  override def typeChecker = _.isInstanceOf[StoreProperty]
}

case class LoadProperty(op: LoadControl.Type) extends InstProperty(op)
object LoadField extends InstEnumField(LoadControl) {
  override def name = "load"
  override def typeChecker = _.isInstanceOf[LoadProperty]
}

case class SignedProperty(op: SignedControl.Type) extends InstProperty(op)
object SignedField extends InstEnumField(SignedControl) {
  override def name = "signed"
  override def typeChecker = _.isInstanceOf[SignedProperty]
}

case class ECallProperty(op: EcallIE.Type) extends InstProperty(op)
object ECallField extends InstEnumField(EcallIE) {
  override def name = "ecall"
  override def typeChecker = _.isInstanceOf[ECallProperty]
}

case class EBreakProperty(op: EbreakIE.Type) extends InstProperty(op)
object EBreakField extends InstEnumField(EbreakIE) {
  override def name = "ebreak"
  override def typeChecker = _.isInstanceOf[EBreakProperty]
}

case class MRetProperty(op: MRetIE.Type) extends InstProperty(op)
object MRetField extends InstEnumField(MRetIE) {
  override def name = "mret"
  override def typeChecker = _.isInstanceOf[MRetProperty]
}

case class SRetProperty(op: Bool) extends InstProperty(op)
object SRetField extends InstBoolField {
  override def name = "sret"
  override def typeChecker = _.isInstanceOf[SRetProperty]
}

case class FenceProperty(op: FenceEnableIE.Type) extends InstProperty(op)
object FenceField extends InstEnumField(FenceEnableIE) {
  override def name = "fence"
  override def typeChecker = _.isInstanceOf[FenceProperty]
}

case class FlushICacheProperty(op: IcacheFlushIE.Type) extends InstProperty(op)
object FlushICacheField extends InstEnumField(IcacheFlushIE) {
  override def name = "flush i cache"
  override def typeChecker = _.isInstanceOf[FlushICacheProperty]
}

case class FlushTLBProperty(op: Bool) extends InstProperty(op)
object FlushTLBField extends InstBoolField {
  override def name = "flush tlb"
  override def typeChecker = _.isInstanceOf[FlushTLBProperty]
}

case class WFIProperty(op: WFIIE.Type) extends InstProperty(op)
object WFIField extends InstEnumField(WFIIE) {
  override def name = "wfi"
  override def typeChecker = _.isInstanceOf[WFIProperty]
}

case class AccAddrProperty(op: AccType.Type) extends InstProperty(op)
object AccAddrField extends InstEnumField(AccType) {
  override def name = "acc addr"
  override def typeChecker = _.isInstanceOf[AccAddrProperty]
}

case class AccUseRdProperty(op: Bool) extends InstProperty(op)
object AccUseRdField extends InstBoolField {
  override def name = "acc use rd"
  override def typeChecker = _.isInstanceOf[AccUseRdProperty]
}

case class AccValidProperty(op: Bool) extends InstProperty(op)
object AccValidField extends InstBoolField {
  override def name = "acc valid"
  override def typeChecker = _.isInstanceOf[AccValidProperty]
}

case class AmoProperty(op: AMOType.Type) extends InstProperty(op)
object AmoField extends InstEnumField(AMOType) {
  override def name = "amo"
  override def typeChecker = _.isInstanceOf[AmoProperty]
}

/**
  * Definitions of Composite properties
  */

case class OpCompProperty(a: OperandType.Type, b: OperandType.Type) extends InstProperty(
  OpAProperty(a) ++
    OpBProperty(b)
)

case class AluCompProperty(alu: ALUControlIE.Type, a: OperandType.Type, b: OperandType.Type) extends InstProperty(
  AluProperty(alu) ++
    OpCompProperty(a, b)
)

case class BranchCompProperty(alu: ALUControlIE.Type) extends InstProperty(
  AluCompProperty(alu, OperandType.Reg, OperandType.Reg) ++
    NextPcProperty(PcType.Branch) ++
    WriteRdProperty(false.B)
)

case class StoreCompProperty(lsSize: DataSize.Type) extends InstProperty(
  OpCompProperty(OperandType.Reg, OperandType.SImmediate) ++
    StoreProperty(StoreControl.EN) ++
    LsSizeProperty(lsSize) ++
    WriteRdProperty(false.B)
)

case class LoadCompProperty(lsSize: DataSize.Type, isSigned: SignedControl.Type = SignedControl.signed) extends InstProperty(
  OpCompProperty(OperandType.Reg, OperandType.IImmediate) ++
    LoadProperty(LoadControl.EN) ++
    SignedProperty(isSigned) ++
    LsSizeProperty(lsSize) ++
    UseRdProperty(true.B)
)

case class RetCompProperty(nextPc: PcType.Type) extends InstProperty(
  WriteRdProperty(false.B) ++
    NextPcProperty(nextPc)
)

abstract trait InstDecode {
  val table: Seq[InstPattern]
}

object RV32IDecode extends InstDecode {
  import klase32.Instructions._

  override val table = Seq(
    new InstPattern(ADD, AluCompProperty(ALUControlIE.ADD, OperandType.Reg, OperandType.Reg)),
    new InstPattern(ADDI,AluCompProperty(ALUControlIE.ADD, OperandType.Reg, OperandType.IImmediate)),
    new InstPattern(SUB, AluCompProperty(ALUControlIE.SUB, OperandType.Reg, OperandType.Reg)),
    new InstPattern(XOR, AluCompProperty(ALUControlIE.XOR, OperandType.Reg, OperandType.Reg)),
    new InstPattern(XORI, AluCompProperty(ALUControlIE.XOR, OperandType.Reg, OperandType.IImmediate)),
    new InstPattern(OR, AluCompProperty(ALUControlIE.OR, OperandType.Reg, OperandType.Reg)),
    new InstPattern(ORI, AluCompProperty(ALUControlIE.OR, OperandType.Reg, OperandType.IImmediate)),
    new InstPattern(AND, AluCompProperty(ALUControlIE.AND, OperandType.Reg, OperandType.Reg)),
    new InstPattern(ANDI, AluCompProperty(ALUControlIE.AND, OperandType.Reg, OperandType.IImmediate)),
    new InstPattern(SLT, AluCompProperty(ALUControlIE.SLT, OperandType.Reg, OperandType.Reg)),
    new InstPattern(SLTI, AluCompProperty(ALUControlIE.SLT, OperandType.Reg, OperandType.IImmediate)),
    new InstPattern(SLTU, AluCompProperty(ALUControlIE.SLTU, OperandType.Reg, OperandType.Reg)),
    new InstPattern(SLTIU, AluCompProperty(ALUControlIE.SLTU, OperandType.Reg, OperandType.IImmediate)),
    new InstPattern(SLL, AluCompProperty(ALUControlIE.SLL, OperandType.Reg, OperandType.Reg)),
    new InstPattern(SLLI, AluCompProperty(ALUControlIE.SLL, OperandType.Reg, OperandType.IImmediate)),
    new InstPattern(SRL, AluCompProperty(ALUControlIE.SRL, OperandType.Reg, OperandType.Reg)),
    new InstPattern(SRLI, AluCompProperty(ALUControlIE.SRL, OperandType.Reg, OperandType.IImmediate)),
    new InstPattern(SRA, AluCompProperty(ALUControlIE.SRA, OperandType.Reg, OperandType.Reg)),
    new InstPattern(SRAI, AluCompProperty(ALUControlIE.SRA, OperandType.Reg, OperandType.IImmediate)),

    new InstPattern(LUI, AluCompProperty(ALUControlIE.ADD, OperandType.Reg, OperandType.UImmediate)),
    new InstPattern(AUIPC, AluCompProperty(ALUControlIE.ADD, OperandType.PC, OperandType.UImmediate)),

    new InstPattern(JAL,
      OpCompProperty(OperandType.PC, OperandType.JImmediate) ++
        RdProperty(RdType.ConsecPC) ++
        NextPcProperty(PcType.Alu) ++
        W1WritebackProperty(W1WritebackIE.EN)
    ),

    new InstPattern(JALR,
      OpCompProperty(OperandType.Reg, OperandType.IImmediate) ++
        RdProperty(RdType.ConsecPC) ++
        NextPcProperty(PcType.Alu)
    ),

    new InstPattern(BEQ, BranchCompProperty(ALUControlIE.EQ)),
    new InstPattern(BNE, BranchCompProperty(ALUControlIE.NE)),
    new InstPattern(BLT, BranchCompProperty(ALUControlIE.SLT)),
    new InstPattern(BLTU, BranchCompProperty(ALUControlIE.SLTU)),
    new InstPattern(BGE, BranchCompProperty(ALUControlIE.GE)),
    new InstPattern(BGEU, BranchCompProperty(ALUControlIE.GEU)),

    new InstPattern(SB, StoreCompProperty(DataSize.Byte)),
    new InstPattern(SH, StoreCompProperty(DataSize.HalfWord)),
    new InstPattern(SW, StoreCompProperty(DataSize.Word)),

    new InstPattern(LB, LoadCompProperty(DataSize.Byte)),
    new InstPattern(LH, LoadCompProperty(DataSize.HalfWord)),
    new InstPattern(LW, LoadCompProperty(DataSize.Word)),
    new InstPattern(LBU, LoadCompProperty(DataSize.Byte, SignedControl.unsigned)),
    new InstPattern(LHU, LoadCompProperty(DataSize.HalfWord, SignedControl.unsigned)),

    new InstPattern(CSRRW,
      OpCompProperty(OperandType.Reg, OperandType.None) ++
        CSRControlProperty(CSRControl.RW) ++
        RdProperty(RdType.BypassCSR)
    ),
    new InstPattern(CSRRWI,
      OpCompProperty(OperandType.CSRImmediate, OperandType.None) ++
        CSRControlProperty(CSRControl.RW) ++
        RdProperty(RdType.BypassCSR)
    ),
    new InstPattern(CSRRS,
      OpCompProperty(OperandType.Reg, OperandType.None) ++
        CSRControlProperty(CSRControl.RS) ++
        RdProperty(RdType.BypassCSR)
    ),
    new InstPattern(CSRRSI,
      OpCompProperty(OperandType.CSRImmediate, OperandType.None) ++
        CSRControlProperty(CSRControl.RS) ++
        RdProperty(RdType.BypassCSR)
    ),
    new InstPattern(CSRRC,
      OpCompProperty(OperandType.Reg, OperandType.None) ++
        CSRControlProperty(CSRControl.RC) ++
        RdProperty(RdType.BypassCSR)
    ),
    new InstPattern(CSRRCI,
      OpCompProperty(OperandType.CSRImmediate, OperandType.None) ++
        CSRControlProperty(CSRControl.RC) ++
        RdProperty(RdType.BypassCSR)
    ),

    new InstPattern(ECALL, ECallProperty(EcallIE.EN)),
    new InstPattern(EBREAK, EBreakProperty(EbreakIE.EN)),

    new InstPattern(SRET, RetCompProperty(PcType.SRet)
      ++ SRetProperty(true.B)),
    new InstPattern(MRET, RetCompProperty(PcType.MRet)
      ++ MRetProperty(MRetIE.EN)),

    new InstPattern(FENCE,
      WriteRdProperty(false.B) ++
        FenceProperty(FenceEnableIE.EN)
    ),
    new InstPattern(FENCE_I, FlushICacheProperty(IcacheFlushIE.EN)),
    new InstPattern(SFENCE_VMA, FlushTLBProperty(true.B)),
    new InstPattern(WFI, WFIProperty(WFIIE.EN)),
  )
}


case class AmoCompProperty(amo: AMOType.Type) extends InstProperty(
  // AluCompProperty(ALUControlIE.BypassA, OperandType.Reg, OperandType.Reg) ++
  OpCompProperty(OperandType.Reg, OperandType.Reg) ++
    WriteRdProperty(false.B) ++
    UseRdProperty(true.B) ++
    LoadProperty(LoadControl.EN) ++
    SignedProperty(SignedControl.signed) ++
    LsSizeProperty(DataSize.Word) ++
    AmoProperty(amo)
)

object RV32ADecode extends InstDecode {
  import klase32.Instructions._

  override val table = Seq(
    AMOADD_W  -> AMOType.Add,
    AMOXOR_W  -> AMOType.Xor,
    AMOOR_W   -> AMOType.Or,
    AMOAND_W  -> AMOType.And,
    AMOMIN_W  -> AMOType.Min,
    AMOMAX_W  -> AMOType.Max,
    AMOMINU_W -> AMOType.Minu,
    AMOMAXU_W -> AMOType.Maxu,
    AMOSWAP_W -> AMOType.Swap,
    LR_W      -> AMOType.LR,
    SC_W      -> AMOType.SC,
  ).map {
    case (k,v) => new InstPattern(k, AmoCompProperty(v))
  }
}


case class MulDivCompProperty() extends InstProperty(
  OpCompProperty(OperandType.Reg, OperandType.Reg) ++
    WriteRdProperty(false.B) ++
    UseRdProperty(true.B) ++
    AccValidProperty(true.B) ++
    AccUseRdProperty(true.B) ++
    AccAddrProperty(AccType.SharedMulDiv)
)

object RV32MDecode extends InstDecode {
  import klase32.Instructions._

  override val table = Seq(
    MUL,
    MULH,
    MULHSU,
    MULHU,
    DIV,
    DIVU,
    REM,
    REMU,
  ).map {
    new InstPattern(_, MulDivCompProperty())
  }
}
