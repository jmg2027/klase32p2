package klase32

import chisel3._
import chisel3.util._

import chisel3.util.experimental.decode.
{DecodeField, DecodePattern, BoolDecodeField, DecodeTable, TruthTable}
import klase32.include.enums._
import klase32.include.ControlSignal._


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

trait DecodeFieldWithDefault[T <: DecodePattern, D <: Data] extends DecodeField[T, D] {
  def default: BitPat
}

trait BoolDecodeFieldWithDefault[T <: DecodePattern] extends DecodeFieldWithDefault[T, Bool] {
  def chiselType: Bool = Bool()

  def y: BitPat = BitPat.Y(1)

  def n: BitPat = BitPat.N(1)
}

abstract class InstEnumField(e: SnitchEnum) extends DecodeFieldWithDefault[InstPattern, EnumType] {
  //  override def default = BitPat(e.default.litOption.get.U(e.getWidth.W))
  def default = BitPat(e.default.litOption.get.U(e.getWidth.W))
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

abstract class InstBoolField(_default: Bool = false.B) extends BoolDecodeFieldWithDefault[InstPattern] {
  def default = BitPat(_default) // Every default value is 0
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

// Custom extension for decode
class DecodeTableWithDefault[I <: DecodePattern, D <: Data, T <: DecodeFieldWithDefault[I, _]](patterns: Seq[I], fields: Seq[T])
  extends DecodeTable[I](patterns, fields) {
  private val _table = patterns.map { op =>
    op.bitPat -> fields.reverse.map(field => field.genTable(op)).reduce(_ ## _)
  }
  private val _default = fields.reverse.map(field => field.default).reduce(_ ## _)

  override val table: TruthTable = TruthTable(_table, _default)
}

/**
 * Definitions of Decoder Properties and Fields
 */

case class IllegalProperty(op: IllegalInstIE.Type) extends InstProperty(op)
object IllegalField extends InstEnumField(IllegalInstIE) {
  override def name = "illegal"
  override def default = BitPat(IllegalInstIE.illegal.litOption.get.U(IllegalInstIE.getWidth.W))
  override def typeChecker = _.isInstanceOf[IllegalProperty]
}
//case class IllegalProperty(f: Bool) extends InstProperty(f)
//object IllegalField extends InstBoolField(false.B) {
//  override def name = "illegal"
//  override def default = y
//  override def typeChecker = _.isInstanceOf[IllegalProperty]
//}

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

case class W0WritebackProperty(op: W0WritebackIE.Type) extends InstProperty(op)
object W0WritebackField extends InstEnumField(W0WritebackIE) {
  override def name = "w1 writeback"
  override def typeChecker = _.isInstanceOf[W0WritebackProperty]
}

case class UseRdProperty(op: Bool) extends InstProperty(op)
object UseRdField extends InstBoolField {
  override def name = "use rd"
  override def typeChecker = _.isInstanceOf[UseRdProperty]
}

case class RS2NotALUBProperty(value: RS2NotALUBIE.Type)(op: Bool) extends InstProperty(op)
object RS2NotALUBProperty extends InstBoolField {
  override def name = "use rd"
  override def typeChecker = _.isInstanceOf[RS2NotALUBProperty]
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

// for debugger
case class DRetProperty(op: DRetIE.Type) extends InstProperty(op)
object DRetField extends InstEnumField(DRetIE) {
  override def name = "dret"
  override def typeChecker: InstProperty => Boolean = _.isInstanceOf[DRetProperty]
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
  override def name = "wfiOut"
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
  override def name = "acc start"
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
  OpCompProperty(a, b) ++
  W0WritebackProperty(W0WritebackIE.EN)
)

case class BranchCompProperty(alu: ALUControlIE.Type) extends InstProperty(
  AluProperty(alu) ++
  OpCompProperty(OperandType.Reg, OperandType.Reg) ++
  CtrlControlIEProperty(FrontendControlIE.BR)
  //    RS2NotALUBProperty(RS2NotALUBIE.EN)
)

case class StoreCompProperty(lsSize: DataSize.Type) extends InstProperty(
  OpCompProperty(OperandType.Reg, OperandType.SImmediate) ++
    AluProperty(ALUControlIE.ADD) ++
    StoreProperty(StoreControl.EN) ++
    LsSizeProperty(lsSize)
)

case class LoadCompProperty(lsSize: DataSize.Type, isSigned: SignedControl.Type = SignedControl.signed) extends InstProperty(
  OpCompProperty(OperandType.Reg, OperandType.IImmediate) ++
    AluProperty(ALUControlIE.ADD) ++
    LoadProperty(LoadControl.EN) ++
    SignedProperty(isSigned) ++
    LsSizeProperty(lsSize) ++
    UseRdProperty(true.B)
)

case class JumpCompProperty(frontendControl: FrontendControlIE.Type) extends InstProperty(
  AluProperty(ALUControlIE.ADD) ++
    OpCompProperty.tupled(
      {
        frontendControl match {
          case FrontendControlIE.JAL => (OperandType.PC, OperandType.JImmediate)
          case FrontendControlIE.JALR => (OperandType.Reg, OperandType.IImmediate)
        }
      }
    ) ++
    RdProperty(RdType.ConsecPC) ++
    CtrlControlIEProperty(frontendControl) ++
    W0WritebackProperty(W0WritebackIE.EN)
)

case class RetCompProperty(frontendControl: FrontendControlIE.Type) extends InstProperty(
  W0WritebackProperty(W0WritebackIE.EN) ++
    CtrlControlIEProperty(frontendControl)
)

abstract trait InstDecode {
  val table: Seq[InstPattern]
}

object RV32IDecode extends InstDecode {
  import klase32.include.Instructions._

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

    new InstPattern(LUI, AluCompProperty(ALUControlIE.ADD, OperandType.None, OperandType.UImmediate)),
    new InstPattern(AUIPC, AluCompProperty(ALUControlIE.ADD, OperandType.PC, OperandType.UImmediate)),

    new InstPattern(JAL,
      //      AluProperty(ALUControlIE.ADD) ++
      //      OpCompProperty(OperandType.PC, OperandType.JImmediate) ++
      //      RdProperty(RdType.ConsecPC) ++
      //      CtrlControlIEProperty(FrontendControlIE.JAL) ++
      //      W0WritebackProperty(W0WritebackIE.EN)
      JumpCompProperty(FrontendControlIE.JAL)
    ),

    new InstPattern(JALR,
      //      AluProperty(ALUControlIE.ADD) ++
      //      OpCompProperty(OperandType.Reg, OperandType.IImmediate) ++
      //      RdProperty(RdType.ConsecPC) ++
      //      CtrlControlIEProperty(FrontendControlIE.JALR) ++
      //      W0WritebackProperty(W0WritebackIE.EN)
      JumpCompProperty(FrontendControlIE.JALR)
    ),

    new InstPattern(BEQ, BranchCompProperty(ALUControlIE.EQ)),
    new InstPattern(BNE, BranchCompProperty(ALUControlIE.NE)),
    new InstPattern(BLT, BranchCompProperty(ALUControlIE.LT)),
    new InstPattern(BLTU, BranchCompProperty(ALUControlIE.LTU)),
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

    // FIXME: CSR does not need to use alu
    new InstPattern(CSRRW,
      AluProperty(ALUControlIE.ADD) ++
        OpCompProperty(OperandType.Reg, OperandType.None) ++
        CSRControlProperty(CSRControl.RW) ++
        W0WritebackProperty(W0WritebackIE.EN) ++
        RdProperty(RdType.BypassCSR)
    ),
    new InstPattern(CSRRWI,
      AluProperty(ALUControlIE.ADD) ++
        OpCompProperty(OperandType.CSRImmediate, OperandType.None) ++
        CSRControlProperty(CSRControl.RW) ++
        W0WritebackProperty(W0WritebackIE.EN) ++
        RdProperty(RdType.BypassCSR)
    ),
    new InstPattern(CSRRS,
      AluProperty(ALUControlIE.ADD) ++
        OpCompProperty(OperandType.Reg, OperandType.None) ++
        CSRControlProperty(CSRControl.RS) ++
        W0WritebackProperty(W0WritebackIE.EN) ++
        RdProperty(RdType.BypassCSR)
    ),
    new InstPattern(CSRRSI,
      AluProperty(ALUControlIE.ADD) ++
        OpCompProperty(OperandType.CSRImmediate, OperandType.None) ++
        CSRControlProperty(CSRControl.RS) ++
        W0WritebackProperty(W0WritebackIE.EN) ++
        RdProperty(RdType.BypassCSR)
    ),
    new InstPattern(CSRRC,
      AluProperty(ALUControlIE.ADD) ++
        OpCompProperty(OperandType.Reg, OperandType.None) ++
        CSRControlProperty(CSRControl.RC) ++
        W0WritebackProperty(W0WritebackIE.EN) ++
        RdProperty(RdType.BypassCSR)
    ),
    new InstPattern(CSRRCI,
      AluProperty(ALUControlIE.ADD) ++
        OpCompProperty(OperandType.CSRImmediate, OperandType.None) ++
        CSRControlProperty(CSRControl.RC) ++
        W0WritebackProperty(W0WritebackIE.EN) ++
        RdProperty(RdType.BypassCSR)
    ),

    new InstPattern(ECALL, ECallProperty(EcallIE.EN)),
    new InstPattern(EBREAK, EBreakProperty(EbreakIE.EN)),

    new InstPattern(SRET, RetCompProperty(FrontendControlIE.MRET)
      ++ SRetProperty(true.B)),
    new InstPattern(MRET, RetCompProperty(FrontendControlIE.MRET)
      ++ MRetProperty(MRetIE.EN)),
    // for debugger
    new InstPattern(DRET, RetCompProperty(FrontendControlIE.DRET)
      ++ DRetProperty(DRetIE.EN)),

    new InstPattern(FENCE,
      W0WritebackProperty(W0WritebackIE.EN) ++
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
    //    W1WritebackProperty(W1WritebackIE.EN) ++
    UseRdProperty(true.B) ++
    LoadProperty(LoadControl.EN) ++
    SignedProperty(SignedControl.signed) ++
    LsSizeProperty(DataSize.Word) ++
    AmoProperty(amo)
)

object RV32ADecode extends InstDecode {
  import klase32.include.Instructions._

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
    W0WritebackProperty(W0WritebackIE.EN) ++
    UseRdProperty(true.B)
//    AccValidProperty(true.B) ++
//    AccUseRdProperty(true.B) ++
//    AccAddrProperty(AccType.SharedMulDiv)
)

case class MPYProperty(op: MPYControlIE.Type) extends InstProperty(op)
object MPYField extends InstEnumField(MPYControlIE) {
  override def name = "mpy"

  override def typeChecker: InstProperty => Boolean = _.isInstanceOf[MPYProperty]
}

case class MPYCtrlProperty(op: MPYControlIE.Type) extends InstProperty(
  OpCompProperty(OperandType.Reg, OperandType.Reg) ++
    W0WritebackProperty(W0WritebackIE.EN) ++
    UseRdProperty(true.B) ++
    MPYProperty(op)
)

case class DIVProperty(op: DIVControlIE.Type) extends InstProperty(op)
object DIVField extends InstEnumField(DIVControlIE) {
  override def name = "div"

  override def typeChecker: InstProperty => Boolean = _.isInstanceOf[DIVProperty]
}

case class DIVCtrlProperty(op: DIVControlIE.Type) extends InstProperty(
  OpCompProperty(OperandType.Reg, OperandType.Reg) ++
    W0WritebackProperty(W0WritebackIE.EN) ++
    UseRdProperty(true.B) ++
    DIVProperty(op)
)

object RV32MDecode extends InstDecode {

  import klase32.include.Instructions._

  override val table = Seq(
    new InstPattern(MUL, MPYCtrlProperty(MPYControlIE.MUL)),
    new InstPattern(MULH, MPYCtrlProperty(MPYControlIE.MULH)),
    new InstPattern(MULHSU, MPYCtrlProperty(MPYControlIE.MULHSU)),
    new InstPattern(MULHU, MPYCtrlProperty(MPYControlIE.MULHU)),
    new InstPattern(DIV, DIVCtrlProperty(DIVControlIE.DIV)),
    new InstPattern(DIVU, DIVCtrlProperty(DIVControlIE.DIVU)),
    new InstPattern(REM, DIVCtrlProperty(DIVControlIE.REM)),
    new InstPattern(REMU, DIVCtrlProperty(DIVControlIE.REMU))
  )
}
