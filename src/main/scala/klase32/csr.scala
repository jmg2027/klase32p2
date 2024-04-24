package klase32

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import klase32.config._
import klase32.param.KlasE32ParamKey

import scala.collection.mutable

// from rocket csr
class MStatus extends Bundle {
  val sd = Bool()
  val zero4 = UInt(23.W)
  val mpv = Bool()
  val gva = Bool()
  val mbe = Bool()
  val sbe = Bool()
  val sxl = UInt(2.W)
  val uxl = UInt(2.W) // uppers are for rv64
  val sd_rv32 = Bool()
  val zero3 = UInt(8.W)
  val tsr = Bool()
  val tw = Bool()
  val tvm = Bool()
  val mxr = Bool()
  val sum = Bool()
  val mprv = Bool()
  val xs = UInt(2.W)
  val fs = UInt(2.W)
  val mpp = UInt(2.W)
  val vs = UInt(2.W)
  val spp = Bool()
  val mpie = Bool()
  val ube = Bool()
  val spie = Bool()
  // val upie = Bool() // disappeared in v1.12
  val zero2 = Bool()
  val mie = Bool()
  // val hie = Bool() // disappeared in v1.12
  val zero1 = Bool()
  val sie = Bool()
  // val uie = Bool() // disappeared in v1.12
  val zero0 = Bool()
}

// v1.12
class MStatush extends Bundle {
  val mbe = Bool()
  val sbe = Bool()
  val zero0 = UInt(4.W)
}

// maybe can use this later
/*
class DCSR extends Bundle {
  val xdebugver = UInt(2.W)
  val zero4 = UInt(2.W)
  val zero3 = UInt(12.W)
  val ebreakm = Bool()
  val ebreakh = Bool()
  val ebreaks = Bool()
  val ebreaku = Bool()
  val zero2 = Bool()
  val stopcycle = Bool()
  val stoptime = Bool()
  val cause = UInt(3.W)
  val v = Bool()
  val zero1 = UInt(2.W)
  val step = Bool()
  val prv = UInt(PRV.SZ.W)
}
 */

class MIP(implicit p: Parameters) extends CoreBundle()(p)
  with HasCoreParameters {
  val zero7 = Bool()
  val debug = Bool() // keep in sync with CSR.debugIntCause
  val zero6 = UInt(2.W)
  val meip = Bool()
  val zero5 = Bool()
  // val vseip = Bool() // spec out v1.12
  val seip = Bool()
  val zero4 = Bool()
  // val ueip = Bool() // spec out v1.12
  val mtip = Bool()
  val zero3 = Bool()
  // val vstip = Bool() // spec out v1.12
  val stip = Bool()
  val zero2 = Bool()
  // val utip = Bool() // spec out v1.12
  val msip = Bool()
  val zero1 = Bool()
  // val vssip = Bool() // spec out v1.12
  val ssip = Bool()
  val zero0 = Bool()
  // val usip = Bool() // spec out v1.12
}

class Envcfg extends Bundle {
  val stce = Bool() // only for menvcfg/henvcfg
  val pbmte = Bool() // only for menvcfg/henvcfg
  val zero54 = UInt(54.W)
  // upper is envcfgh
  val cbze = Bool()
  val cbcfe = Bool()
  val cbie = UInt(2.W)
  val zero3 = UInt(3.W)
  val fiom = Bool()
  def write(wdata: UInt): Unit = {
    val new_envcfg = wdata.asTypeOf(new Envcfg)
    fiom := new_envcfg.fiom // only FIOM is writable currently
  }
}

object CSRAddr {
  val mstatus = freechips.rocketchip.rocket.CSRs.mstatus
  val mstatush = freechips.rocketchip.rocket.CSRs.mstatush
  val misa = freechips.rocketchip.rocket.CSRs.misa
  val medeleg = freechips.rocketchip.rocket.CSRs.medeleg
  val mideleg = freechips.rocketchip.rocket.CSRs.mideleg
  val mie = freechips.rocketchip.rocket.CSRs.mie
  val mtvec = freechips.rocketchip.rocket.CSRs.mtvec
  val mvendorid = freechips.rocketchip.rocket.CSRs.mvendorid
  val marchid = freechips.rocketchip.rocket.CSRs.marchid
  val mimpid = freechips.rocketchip.rocket.CSRs.mimpid
  val mhartid = freechips.rocketchip.rocket.CSRs.mhartid
  val mscratch = freechips.rocketchip.rocket.CSRs.mscratch
  val mepc = freechips.rocketchip.rocket.CSRs.mepc
  val mcause = freechips.rocketchip.rocket.CSRs.mcause
  val mtval = freechips.rocketchip.rocket.CSRs.mtval
  val mip = freechips.rocketchip.rocket.CSRs.mip
  val mconfigptr = freechips.rocketchip.rocket.CSRs.mconfigptr
  val menvcfg = freechips.rocketchip.rocket.CSRs.menvcfg
  val menvcfgh = freechips.rocketchip.rocket.CSRs.menvcfgh
  val mseccfg = freechips.rocketchip.rocket.CSRs.mseccfg
}

// Reference: Rocket core
class CSRList(implicit p: Parameters) extends CoreBundle with HasCoreParameters {
  val mstatus = RegInit(0.U.asTypeOf(new MStatus))
  val mstatush = UInt(csrWidthM.W)
  val misa = UInt(csrWidthM.W)
  val medeleg = UInt(csrWidthM.W)
  val mideleg = UInt(csrWidthM.W)
  val mie = UInt(csrWidthM.W)
  val mtvec = UInt(csrWidthM.W)
  val mvendorid = UInt(csrWidthM.W)
  val marchid = UInt(csrWidthM.W)
  val mimpid = UInt(csrWidthM.W)
  val mhartid = UInt(csrWidthM.W)
  val mscratch = UInt(csrWidthM.W)
  val mepc = UInt(csrWidthM.W)
  val mcause = UInt(csrWidthM.W)
  val mtval = UInt(csrWidthM.W)
  val mip = UInt(csrWidthM.W)
  val mconfigptr = UInt(csrWidthM.W)
  val menvcfg = UInt(csrWidthM.W)
  val menvcfgh = UInt(csrWidthM.W)
  val mseccfg = UInt(csrWidthM.W)

  val map = Map[Int, Bits](
    CSRAddr.mstatus -> mstatus,
    CSRAddr.mstatush -> mstatush,
    CSRAddr.misa -> misa,
    CSRAddr.medeleg -> medeleg,
    CSRAddr.mideleg -> mideleg,
    CSRAddr.mie -> mie,
    CSRAddr.mtvec -> mtvec,
    CSRAddr.mvendorid -> mvendorid,
    CSRAddr.marchid -> marchid,
    CSRAddr.mimpid -> mimpid,
    CSRAddr.mhartid -> mhartid,
    CSRAddr.mscratch -> mscratch,
    CSRAddr.mepc -> mepc,
    CSRAddr.mcause -> mcause,
    CSRAddr.mtval -> mtval,
    CSRAddr.mip -> mip,
    CSRAddr.mconfigptr -> mconfigptr,
    CSRAddr.menvcfg -> menvcfg,
    CSRAddr.menvcfgh -> menvcfgh,
    CSRAddr.mseccfg -> mseccfg,
  )
}

class MISA(implicit p: Parameters) extends CoreBundle with HasCoreParameters {
  //def default(implicit p: Parameters): Unit = {
  def default: BigInt = {
    val isaMaskString =
      (if (usingMulDiv) "M" else "") +
      (if (usingAtomics) "A" else "") +
      (if (fLen >= 32) "F" else "") +
      (if (fLen >= 64) "D" else "") +
      (if (usingVector) "V" else "") +
      // The current spec does not define what sub-extensions constitute the 'B' misa bit
      // (if (usingBitManip) "B" else "") +
      (if (usingCompressed) "C" else "")
    val isaString = (if (usingRVE) "E" else "I") +
      isaMaskString +
      (if (customIsaExt.isDefined) "X" else "") +
      (if (usingSupervisor) "S" else "") +
      (if (usingHypervisor) "H" else "") +
      (if (usingUser) "U" else "")
    val isaMax = (BigInt(log2Ceil(mxLen) - 4) << (mxLen - 2)) | isaString.map(x => 1 << (x - 'A')).foldLeft((0))(_|_)
    isaMax
  }
}

object CSRList {
  def apply(csr: CSRList): Unit = {

  }
  def default(implicit p: Parameters) = {
    (new CSRList).Lit(
      _.mstatus -> 0.U.asTypeOf(new MStatus),
      _.mstatush -> 0.U.asTypeOf(new MStatush),
      _.misa -> (new MISA).default.U,
      _.medeleg -> 0.U,
      _.mideleg -> 0.U,
      _.mie -> Interrupt.default,
      _.mtvec -> 0.U,
      _.mvendorid -> 0.U,
      _.marchid -> 0.U,
      _.mimpid -> 0.U,
      _.mhartid -> 0.U, // Can be implemented from csr.io
      _.mscratch -> 0.U,
      // _.mepc -> UInt(csrWidth.W) // No reset value: must set
      _.mcause -> 0.U,
      // _.mtval -> UInt(csrWidth.W)
      _.mip -> Interrupt.default,
      _.mconfigptr -> 0.U,
      _.menvcfg -> 0.U,
      _.menvcfgh -> 0.U,
      _.mseccfg -> 0.U,
    )
  }
}

class CSRCtrl(implicit p: Parameters) extends CoreBundle with HasCoreParameters {
  val in = UInt(csrWidthM.W)
  val inst = CSRInstMuxIE()
  val addr = UInt(12.W)
}

class CSRIntfIO(implicit p: Parameters) extends CoreBundle with HasCoreParameters {
  val ctrl = Input(new CSRCtrl())

  val out = Output(new CSRStat())

  val rd = Output(UInt(csrWidthM.W))

  val interrupt = Input(new Interrupt)
  val excption = Input(Bool())
  val cause = Input(UInt(5.W))

  val hartId = Input(UInt(hartIDWidth.W))

  val br = Input(Bool())
  val jal = Input(Bool())
  val jalr = Input(Bool())
}

class CSRModule(implicit p: Parameters) extends CoreModule {
  val io = IO(new CSRIntfIO)
  val csr = RegInit(CSRStat.default)

}
