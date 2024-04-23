package klase32

import chisel3._
import chisel3.util.BitPat
import chisel3.util._
import chisel3.util.experimental.decode.{DecodeField, DecodePattern, DecodeTable}
import chisel3.experimental.BundleLiterals._
import klase32.config._
import klase32.param.KlasE32ParamKey
import klase32.Instructions._
import snitch.enums._

/*
abstract trait DecodeConstants extends Const with ControlEnumName
{
  val table: Array[(BitPat, List[BitPat])]
}

abstract trait ControlEnumName {
  val alu = ALUControlIE
  val div = DIVControlIE
  val ctrl = CtrlControlIE
  val memDMb = MemDMbControlIE
  val w1 = W1MuxControlIE
  val w1dead = W1deadMuxControlIE
  val aluA = ALUAMuxControlIE
  val aluB = ALUBMuxControlIE
  val of21 = OF21MuxIE
  val of13 = OF13MuxIE
  val target = TargetMuxIE
  val csrIn = CSRInMuxIE
  val csrInst = CSRInstMuxIE
  val mpyMD = MpyMDMuxIE
  val w1addr = W1AddrMuxIE
  val cxw1addr = CXW1AddrMuxIE
  val hzdIE = HazardIE
  val lsu = LSUME
  val hzdME = HazardME

}

class CtrlSigsIE(implicit val p: Parameters) extends Bundle {

  val alu = Bits(ALUControlIE.X.getWidth.W)
  val div = Bits(DIVControlIE.X.getWidth.W)
  val mpy = Bool()
  val ctrl = Bits(CtrlControlIE.X.getWidth.W)
  val regxIE = Bool()
  val memDMb = Bits(MemDMbControlIE.X.getWidth.W)
  val w1 = Bits(W1MuxControlIE.X.getWidth.W)
  // val w1deadMux = Bits(W1deadMuxControlIE.X.getWidth.W) // this is not needed
  val aluA = Bits(ALUAMuxControlIE.X.getWidth.W)
  val aluB = Bits(ALUBMuxControlIE.X.getWidth.W)
  val jalr = Bool()
  val jal = Bool()
  val br = Bool()
  val of21 = Bits(OF21MuxIE.X.getWidth.W)
  val of13 = Bits(OF13MuxIE.X.getWidth.W)
  val target = Bits(TargetMuxIE.X.getWidth.W)
  val csrIn = Bits(CSRInMuxIE.X.getWidth.W)
  val csrInst = Bits(CSRInstMuxIE.X.getWidth.W)
  // val csrro = Bool() // This should be dealt in module: when RS/C*, and rs1 is x0 -> enable
  val fence = Bool()
  // fenceen, fm, pred, suc are triggered by fence
  //val fenceFm = Bool()
  //val fencePred = Bool()
  //val fenceSuc = Bool()
  val icacheflush = Bool()
  val mpyMD = Bits(MpyMDMuxIE.X.getWidth.W)
  val ocdSwbreak = Bool()
  val w1addr = Bits(W1AddrMuxIE.X.getWidth.W)
  val cxw1addr = Bits(CXW1AddrMuxIE.X.getWidth.W)
  val hzdIE = Bits(HazardIE.X.getWidth.W)
}

class CtrlSigsME(implicit val p: Parameters) extends Bundle {
  val lsu = Bits(LSUME.X.getWidth.W)
  val regxME = Bool()
  val hzdME = Bits(HazardME.X.getWidth.W)
}

class CtrlSigs(implicit val p: Parameters) extends Bundle {
  val IE = new CtrlSigsIE()
  val ME = new CtrlSigsME()


  def default: List[BitPat] = {

    List(ALUControlIE.X, DIVControlIE.X, MPYControlIE.X, CtrlControlIE.X, RegXControlIE.X,
      MemDMbControlIE.X, W1MuxControlIE.X, ALUAMuxControlIE.X,
      ALUBMuxControlIE.X, JALRIE.X, JALIE.X, BRIE.X, OF21MuxIE.X, OF13MuxIE.X, TargetMuxIE.X,
      CSRInMuxIE.X, CSRInstMuxIE.X, FenceEnableIE.X,
      IcacheFlushIE.X, MpyMDMuxIE.X, OCDSwbreakMuxIE.X, W1AddrMuxIE.X, CXW1AddrMuxIE.X,
      HazardIE.X, LSUME.X, RegXControlME.X, HazardME.X)
  }


  def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]) = {
    val decoder = freechips.rocketchip.rocket.DecodeLogic(inst, default, table)
    val sigs = Seq(IE.alu, IE.div, IE.mpy, IE.ctrl, IE.regxIE, IE.memDMb, IE.w1, IE.aluA, IE.aluB,
      IE.jalr, IE.jal, IE.br, IE.of21, IE.of13, IE.target, IE.csrIn, IE.csrInst, IE.fence,
      IE.icacheflush, IE.mpyMD, IE.ocdSwbreak, IE.w1addr, IE.cxw1addr, IE.hzdIE,
      ME.lsu, ME.regxME, ME.hzdME)
    sigs zip decoder map {case(s,d) => s := d}
    this
  }
}

class IDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    // Need to check if X is okay
    BNE->       List(alu.NE,  div.X,N , ctrl.BR  ,N , memDMb.X  , w1.X   , aluA.R1, aluB.R2     , N, N, Y, of21.CJ, of13.B, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.X  , cxw1addr.X, hzdIE.RS1RS2, lsu.X, N, hzdME.X ),
    BEQ->       List(alu.EQ,  div.X,N , ctrl.BR  ,N , memDMb.X  , w1.X   , aluA.R1, aluB.R2     , N, N, Y, of21.CJ, of13.B, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.X  , cxw1addr.X, hzdIE.RS1RS2, lsu.X, N, hzdME.X ),
    BLT->       List(alu.LT,  div.X,N , ctrl.BR  ,N , memDMb.X  , w1.X   , aluA.R1, aluB.R2     , N, N, Y, of21.CJ, of13.B, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.X  , cxw1addr.X, hzdIE.RS1RS2, lsu.X, N, hzdME.X ),
    BLTU->      List(alu.LTU, div.X,N , ctrl.BR  ,N , memDMb.X  , w1.X   , aluA.R1, aluB.R2     , N, N, Y, of21.CJ, of13.B, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.X  , cxw1addr.X, hzdIE.RS1RS2, lsu.X, N, hzdME.X ),
    BGE->       List(alu.GE,  div.X,N , ctrl.BR  ,N , memDMb.X  , w1.X   , aluA.R1, aluB.R2     , N, N, Y, of21.CJ, of13.B, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.X  , cxw1addr.X, hzdIE.RS1RS2, lsu.X, N, hzdME.X ),
    BGEU->      List(alu.GEU, div.X,N , ctrl.BR  ,N , memDMb.X  , w1.X   , aluA.R1, aluB.R2     , N, N, Y, of21.CJ, of13.B, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.X  , cxw1addr.X, hzdIE.RS1RS2, lsu.X, N, hzdME.X ),

    JAL->       List(alu.X,   div.X,N , ctrl.JAL ,N , memDMb.X  , w1.LNK , aluA.PC, aluB.R2     , N, Y, N, of21.J , of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.RD , cxw1addr.X, hzdIE.RD , lsu.X, N, hzdME.X ),
    JALR->      List(alu.ADD, div.X,N , ctrl.JALR,N , memDMb.X  , w1.LNK , aluA.R1, aluB.R2     , Y, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.RD , cxw1addr.X, hzdIE.RS1RD , lsu.X, N, hzdME.X ),
    AUIPC->     List(alu.ADD, div.X,N , ctrl.X   ,N , memDMb.X  , w1.ALUR, aluA.PC, aluB.UImm   , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.RD , cxw1addr.X, hzdIE.RD , lsu.X, N, hzdME.X ),

    LB->        List(alu.ADD, div.X,N , ctrl.X   ,N , memDMb.LDB, w1.X   , aluA.R1, aluB.R2     , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.X  , cxw1addr.X, hzdIE.W2RD  , lsu.BS, Y, hzdME.RD ),
    LH->        List(alu.ADD, div.X,N , ctrl.X   ,N , memDMb.LDH, w1.X   , aluA.R1, aluB.R2     , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.X  , cxw1addr.X, hzdIE.W2RD  , lsu.HS, Y, hzdME.RD ),
    LW->        List(alu.ADD, div.X,N , ctrl.X   ,N , memDMb.LDW, w1.X   , aluA.R1, aluB.R2     , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.X  , cxw1addr.X, hzdIE.W2RD  , lsu.default, Y, hzdME.RD ),
    LBU->       List(alu.ADD, div.X,N , ctrl.X   ,N , memDMb.LDB, w1.X   , aluA.R1, aluB.R2     , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.X  , cxw1addr.X, hzdIE.W2RD  , lsu.BU, Y, hzdME.RD ),
    LHU->       List(alu.ADD, div.X,N , ctrl.X   ,N , memDMb.LDH, w1.X   , aluA.R1, aluB.R2     , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.X  , cxw1addr.X, hzdIE.W2RD  , lsu.HU, Y, hzdME.RD ),
    SB->        List(alu.ADD, div.X,N , ctrl.X   ,N , memDMb.STB, w1.X   , aluA.R1, aluB.SImm   , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.X  , cxw1addr.X, hzdIE.RS2, lsu.X, N, hzdME.X ),
    SH->        List(alu.ADD, div.X,N , ctrl.X   ,N , memDMb.STH, w1.X   , aluA.R1, aluB.SImm   , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.X  , cxw1addr.X, hzdIE.RS2, lsu.X, N, hzdME.X ),
    SW->        List(alu.ADD, div.X,N , ctrl.X   ,N , memDMb.STW, w1.X   , aluA.R1, aluB.SImm   , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.X  , cxw1addr.X, hzdIE.RS2, lsu.X, N, hzdME.X ),

    LUI->       List(alu.ADD, div.X,N , ctrl.X   ,Y , memDMb.X  , w1.ALUR, aluA.R1, aluB.UImm   , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.RD , cxw1addr.X, hzdIE.RD  , lsu.X, N, hzdME.X ),
    ADDI->      List(alu.ADD, div.X,N , ctrl.X   ,Y , memDMb.X  , w1.ALUR, aluA.R1, aluB.IImm   , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.RD , cxw1addr.X, hzdIE.RS1RD, lsu.X, N, hzdME.X ),
    SLTI ->     List(alu.SLT, div.X,N , ctrl.X   ,Y , memDMb.X  , w1.ALUR, aluA.R1, aluB.IImm   , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.RD , cxw1addr.X, hzdIE.RS1RD, lsu.X, N, hzdME.X ),
    SLTIU->     List(alu.SLTU,div.X,N , ctrl.X   ,Y , memDMb.X  , w1.ALUR, aluA.R1, aluB.IImm   , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.RD , cxw1addr.X, hzdIE.RS1RD, lsu.X, N, hzdME.X ),
    SLLI ->     List(alu.SLL, div.X, N, ctrl.X   ,Y , memDMb.X  , w1.ALUR, aluA.R1, aluB.IImmUSh, N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.RD , cxw1addr.X, hzdIE.RS1RD, lsu.X, N, hzdME.X ),
    SRLI ->     List(alu.SRL, div.X, N, ctrl.X   ,Y , memDMb.X  , w1.ALUR, aluA.R1, aluB.IImmUSh, N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.RD , cxw1addr.X, hzdIE.RS1RD, lsu.X, N, hzdME.X ),
    SRAI ->     List(alu.SRA, div.X, N, ctrl.X   ,Y , memDMb.X  , w1.ALUR, aluA.R1, aluB.IImmUSh, N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.RD , cxw1addr.X, hzdIE.RS1RD, lsu.X, N, hzdME.X ),
    ANDI->      List(alu.AND, div.X,N , ctrl.X   ,Y , memDMb.X  , w1.ALUR, aluA.R1, aluB.IImm   , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.RD , cxw1addr.X, hzdIE.RS1RD, lsu.X, N, hzdME.X ),
    ORI->       List(alu.OR,  div.X,N , ctrl.X   ,Y , memDMb.X  , w1.ALUR, aluA.R1, aluB.IImm   , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.RD , cxw1addr.X, hzdIE.RS1RD, lsu.X, N, hzdME.X ),
    ORIU->      List(alu.OR,  div.X,N , ctrl.X   ,Y , memDMb.X  , w1.ALUR, aluA.R1, aluB.IImmU  , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.RD , cxw1addr.X, hzdIE.RS1RD, lsu.X, N, hzdME.X ),
    XORI->      List(alu.XOR, div.X,N , ctrl.X   ,Y , memDMb.X  , w1.ALUR, aluA.R1, aluB.IImm   , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.RD , cxw1addr.X, hzdIE.RS1RD, lsu.X, N, hzdME.X ),
    ADD->       List(alu.ADD, div.X,N , ctrl.X   ,Y , memDMb.X  , w1.ALUR, aluA.R1, aluB.R2     , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.RD , cxw1addr.X, hzdIE.RS1RS2RD, lsu.X, N, hzdME.X ),
    SUB->       List(alu.SUB, div.X,N , ctrl.X   ,Y , memDMb.X  , w1.ALUR, aluA.R1, aluB.R2     , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.RD , cxw1addr.X, hzdIE.RS1RS2RD, lsu.X, N, hzdME.X ),
    SLT->       List(alu.SLT, div.X,N , ctrl.X   ,Y , memDMb.X  , w1.ALUR, aluA.R1, aluB.R2     , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.RD , cxw1addr.X, hzdIE.RS1RS2RD, lsu.X, N, hzdME.X ),
    SLTU->      List(alu.SLTU,div.X,N , ctrl.X   ,Y , memDMb.X  , w1.ALUR, aluA.R1, aluB.R2     , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.RD , cxw1addr.X, hzdIE.RS1RS2RD, lsu.X, N, hzdME.X ),
    AND->       List(alu.AND, div.X,N , ctrl.X   ,Y , memDMb.X  , w1.ALUR, aluA.R1, aluB.R2     , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.RD , cxw1addr.X, hzdIE.RS1RS2RD, lsu.X, N, hzdME.X ),
    OR->        List(alu.OR,  div.X,N , ctrl.X   ,Y , memDMb.X  , w1.ALUR, aluA.R1, aluB.R2     , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.RD , cxw1addr.X, hzdIE.RS1RS2RD, lsu.X, N, hzdME.X ),
    XOR->       List(alu.XOR, div.X,N , ctrl.X   ,Y , memDMb.X  , w1.ALUR, aluA.R1, aluB.R2     , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.RD , cxw1addr.X, hzdIE.RS1RS2RD, lsu.X, N, hzdME.X ),
    SLL->       List(alu.SLL, div.X,N , ctrl.X   ,Y , memDMb.X  , w1.ALUR, aluA.R1, aluB.R2     , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.RD , cxw1addr.X, hzdIE.RS1RS2RD, lsu.X, N, hzdME.X ),
    SRL->       List(alu.SRL, div.X,N , ctrl.X   ,Y , memDMb.X  , w1.ALUR, aluA.R1, aluB.R2     , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.RD , cxw1addr.X, hzdIE.RS1RS2RD, lsu.X, N, hzdME.X ),
    SRA->       List(alu.SRA, div.X,N , ctrl.X   ,Y , memDMb.X  , w1.ALUR, aluA.R1, aluB.R2     , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.RD , cxw1addr.X, hzdIE.RS1RS2RD, lsu.X, N, hzdME.X ),

    FENCE->     List(alu.X,   div.X,N , ctrl.X   ,N , memDMb.X  , w1.X   , aluA.PC, aluB.R2     , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , Y, N, mpyMD.X, N, w1addr.X  , cxw1addr.X, hzdIE.X  , lsu.X, N, hzdME.X ),
    FENCE_I->   List(alu.X,   div.X,N , ctrl.X   ,N , memDMb.X  , w1.X   , aluA.PC, aluB.R2     , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, Y, mpyMD.X, N, w1addr.X  , cxw1addr.X, hzdIE.X  , lsu.X, N, hzdME.X ),

    ECALL->     List(alu.X,   div.X,N , ctrl.X   ,N , memDMb.X  , w1.X   , aluA.PC, aluB.R2     , Y, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.X  , cxw1addr.X, hzdIE.X  , lsu.X, N, hzdME.X ),
    EBREAK->    List(alu.X,   div.X,N , ctrl.X   ,N , memDMb.X  , w1.X   , aluA.PC, aluB.R2     , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, Y, w1addr.X  , cxw1addr.X, hzdIE.X  , lsu.X, N, hzdME.X ),
    MRET->      List(alu.X,   div.X,N , ctrl.MRET,N , memDMb.X  , w1.X   , aluA.PC, aluB.R2     , Y, N, N, of21.CJ, of13.X, target.MEPC, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.X  , cxw1addr.X, hzdIE.X  , lsu.X, N, hzdME.X ),
    WFI->       List(alu.X,   div.X,N , ctrl.HALT,N , memDMb.X  , w1.X   , aluA.PC, aluB.R2     , Y, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.X     , N, N, mpyMD.X, N, w1addr.X  , cxw1addr.X, hzdIE.X  , lsu.X, N, hzdME.X ),
    CSRRW->     List(alu.X,   div.X,N , ctrl.X   ,N , memDMb.X  , w1.CSR , aluA.PC, aluB.R2     , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.RW , N, N, mpyMD.X, N, w1addr.RD , cxw1addr.X, hzdIE.RS1RD, lsu.X, N, hzdME.X ),
    CSRRS->     List(alu.X,   div.X,N , ctrl.X   ,N , memDMb.X  , w1.CSR , aluA.PC, aluB.R2     , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.RS , N, N, mpyMD.X, N, w1addr.RD , cxw1addr.X, hzdIE.RS1RD, lsu.X, N, hzdME.X ),
    CSRRC->     List(alu.X,   div.X,N , ctrl.X   ,N , memDMb.X  , w1.CSR , aluA.PC, aluB.R2     , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.R1, csrInst.RC , N, N, mpyMD.X, N, w1addr.RD , cxw1addr.X, hzdIE.RS1RD, lsu.X, N, hzdME.X ),
    CSRRWI->    List(alu.X,   div.X,N , ctrl.X   ,N , memDMb.X  , w1.CSR , aluA.PC, aluB.R2     , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.I , csrInst.RWI, N, N, mpyMD.X, N, w1addr.RD , cxw1addr.X, hzdIE.RD, lsu.X, N, hzdME.X ),
    CSRRSI->    List(alu.X,   div.X,N , ctrl.X   ,N , memDMb.X  , w1.CSR , aluA.PC, aluB.R2     , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.I , csrInst.RSI, N, N, mpyMD.X, N, w1addr.RD , cxw1addr.X, hzdIE.RD, lsu.X, N, hzdME.X ),
    CSRRCI->    List(alu.X,   div.X,N , ctrl.X   ,N , memDMb.X  , w1.CSR , aluA.PC, aluB.R2     , N, N, N, of21.CJ, of13.X, target.ALUR, csrIn.I , csrInst.RCI, N, N, mpyMD.X, N, w1addr.RD , cxw1addr.X, hzdIE.RD, lsu.X, N, hzdME.X ),

    /*
    MUL ->
    MULH ->
    MULHU ->
    MULHSU ->
    MULW ->

    DIV ->
    DIVU ->
    REM -  assign __X_r1_rad_out = en___X_r1_raddr_IE ? __X_r1_raddr_IE :
    r___X_r1_raddr; // __X_r1_raddr
>
    REMU ->
    DIVW ->
    DIVUW ->
    REMW ->
    REMUW ->
     */
  )
}

 */

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

  val csrCtrl = CSRInstMuxIE()

  val rdType = RdType()
  val useRD = Bool()
  val w1Wb = RegXControlIE()
  val w2Wb = RegXControlME()

  val pcCtrl = CtrlControlIE()

  val lsuCtrl = new LSUControl

  val ecall = EcallIE()
  val ebreak = EbreakIE()

  val fence = FenceEnableIE()
  val flushICache = IcacheFlushIE()
  // val flushTLB = Bool()
  val wfi = WFIIE()

  // val divCtrl = DIVControlIE()
  // val mpyCtrl = MpyMDMuxIE()

  val hzdCtrl = HazardIE()
  val hzdLoadCtrl = HazardME()

}

class DecoderIO(implicit p: Parameters) extends CoreBundle {
    val inst = Input(UInt(mxLen.W))

    val decSig = Flipped(Valid(new Decoded()))

  val stall = Input(new Stall())
}

class Decoder(implicit p: Parameters) extends CoreModule {
  val k = p(KlasE32ParamKey)

  val io = IO(new DecoderIO)

  val d = io.decSig.bits
  io.decSig.valid := 1.U

  val decodeMapping = Seq(
    IllegalField -> d.illegal,

    OpAField -> d.operandSelect.a,
    OpBField -> d.operandSelect.b,

    AluField -> d.aluCtrl,

    CSRControlField -> d.csrCtrl,

    RdField -> d.rdType,
    W1WritebackField -> d.w1Wb,
    W2WritebackField -> d.w2Wb,

    NextPcField -> d.pcCtrl,

    LsSizeField -> d.lsuCtrl.lsSize,
    StoreField -> d.lsuCtrl.isStore,
    LoadField -> d.lsuCtrl.isLoad,
    SignedField -> d.lsuCtrl.isSigned,

    ECallField -> d.ecall,
    EBreakField -> d.ebreak,

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

  // Set Output
  decodeMapping.map {
    case(a, b) => b := decodedInst(a)
  }

  val instValue = InstParser(io.inst)
  d.rd := instValue.rd
  d.rs1 := instValue.rs1
  d.rs2 := instValue.rs2

  d.imm.i := instValue.iimm.asSInt
  d.imm.s := instValue.simm.asSInt
  d.imm.b := instValue.bimm.asSInt
  d.imm.u := instValue.uimm.asSInt
  d.imm.j := instValue.jimm.asSInt

  d.hzdCtrl := hazardDecode
  d.hzdLoadCtrl := d.lsuCtrl.isLoad


  def hazardDecode(): HazardIE.Type = {
    import HazardIE._
    val rs1 = d.operandSelect.a === OperandType.Reg
    val rs2 = d.operandSelect.b === OperandType.Reg
    val rd = d.useRD
    val load = d.lsuCtrl.isLoad === LoadControl.EN

    val hzdCond = Seq(rs1 && rs2, rs1 && rd, rd, load, rs1 && rs2 && rd)
    val hzdDec = Seq(RS1RS2, RS1RD, RD, W2RD, RS1RS2RD)

    val hzd = WireDefault(HazardIE(), default)
    hzd := Mux1H(Seq(
      hzdCond(0) -> RS1RS2,
      hzdCond(1) -> RS1RD,
      hzdCond(2) -> RD,
      hzdCond(3) -> W2RD,
      hzdCond(4) -> RS1RS2RD,
    )
    )
    hzd
  }
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
