package klase32

import chisel3._
import chisel3.util._
import klase32.config._
import klase32.param.KLASE32ParamKey
import chisel3.experimental.BundleLiterals._


object CSR {
  abstract class CSRRegTemplate {
//    class field extends Bundle
    def field: Bundle

    def default: UInt

    // Can't I use this as all bundle's type?
//    def reg = RegInit(default.asTypeOf(field))
    lazy val reg: field.type = RegInit(default.asTypeOf(chiselTypeOf(field)))
//    val reg = RegInit(default.asTypeOf(new field))
    def write[T <: Data](wdata: T): Unit = {
      val newData = wdata.asTypeOf(chiselTypeOf(field))
//      val newData = wdata.asTypeOf(chiselTypeOf(new field))
      reg := newData
    }
  }

  abstract class CoreCSRReg(implicit val p: Parameters) extends CSRRegTemplate with HasCoreParameters

  class CSRReg(val defaultValue: UInt = 0.U)(implicit p: Parameters) extends CoreCSRReg {
//    val field = new Bundle {val data = UInt(csrWidthM.W)}
    def field = new Bundle {
      val data = UInt(csrWidthM.W)
    }

    def default = defaultValue
//    override def reg = RegInit(default.asTypeOf(field))
//    override val reg = RegInit(default.asTypeOf(field))
  }


  class MStatus (implicit p: Parameters) extends CoreCSRReg {
//    val field = new Bundle{
    def field = new Bundle{
      //      val sd = Bool()
      //      val zero4 = UInt(23.W)
      //      val mpv = Bool()
      //      val gva = Bool()
      //      val mbe = Bool()
      //      val sbe = Bool()
      //      val sxl = UInt(2.W)
      //      val uxl = UInt(2.W) // uppers are for rv64
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

    def default = 0.U
//    override def reg = RegInit(default.asTypeOf(field))
//    override val reg = RegInit(default.asTypeOf(field))
  }

  // v1.12
  class MStatush(implicit p: Parameters) extends CoreCSRReg {
//    val field = new Bundle{
    def field = new Bundle{
      val mbe = Bool()
      val sbe = Bool()
      val zero0 = UInt(4.W)
    }

    def default = 0.U
//    override def reg = RegInit(default.asTypeOf(field))
//    override val reg = RegInit(default.asTypeOf(field))
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

//  class MIP(implicit p: Parameters) extends CoreCSRReg {
  class MIP(implicit p: Parameters) extends CoreCSRReg {
    def field = new Bundle {
//    class field extends Bundle {
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
    def default = 0.U

//    val reg = RegInit(default.asTypeOf(field))
//    val reg = RegInit(default.asTypeOf(chiselTypeOf(new field)))
  }

  class MIE(implicit p: Parameters) extends MIP

  class Envcfg(implicit p: Parameters) extends CoreCSRReg {
//    val field = new Bundle {
    class field extends Bundle {
      val stce = Bool() // only for menvcfg/henvcfg
      val pbmte = Bool() // only for menvcfg/henvcfg
      val zero54 = UInt(54.W)
      // upper is envcfgh
      val cbze = Bool()
      val cbcfe = Bool()
      val cbie = UInt(2.W)
      val zero3 = UInt(3.W)
      val fiom = Bool()
    }

    def default = 0.U
//    override def reg = RegInit(default.asTypeOf(field))
//    override val reg = RegInit(default.asTypeOf(field))
  }

  class TVec(implicit p: Parameters) extends CoreCSRReg {
    val k = p(KLASE32ParamKey)

//    val field = new Bundle {
    def field = new Bundle {
      val base = UInt((k.core.mxLen-2).W)
      val mode = UInt(2.W)
    }

    def default = 0.U
//    override def reg = RegInit(default.asTypeOf(field))
//    override val reg = RegInit(default.asTypeOf(field))
  }

  // FIXME
  class MISA(implicit p: Parameters) extends CSRReg {
    override def default: UInt = {
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
      val isaMax = (BigInt(log2Ceil(mxLen) - 4) << (mxLen - 2)) |
        isaString.map(x => 1 << (x - 'A'))
          .foldLeft((0))(_ | _)
      isaMax.U
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

  class CSRList(implicit p: Parameters) extends CoreBundle
      with HasCoreParameters {
    val mstatus = new MStatus
    val mstatush = new MStatush
    val misa = new MISA
    val medeleg = new CSRReg
    val mideleg = new CSRReg
    val mie = new MIE
    val mtvec = new TVec
    val mvendorid = new CSRReg
    val marchid = new CSRReg
    val mimpid = new CSRReg
    val mhartid = new CSRReg
    val mscratch = new CSRReg
    val mepc = new CSRReg
    val mcause = new CSRReg
    val mtval = new CSRReg
    val mip = new MIP
    val mconfigptr = new CSRReg
    val menvcfg = new Envcfg
    val menvcfgh = new Envcfg
    val mseccfg = new CSRReg
  }
}

class CSRCtrl(implicit p: Parameters)
  extends CoreBundle
    with HasCoreParameters {
  val in = UInt(csrWidthM.W)
  val inst = CSRInstMuxIE()
  val addr = UInt(12.W)
}

class CSRIntfIO(implicit p: Parameters)
  extends CoreBundle
    with HasCoreParameters {
  val ctrl = Input(new CSRCtrl())

  val out = Output(new CSR.CSRList())

  val rd = Output(UInt(csrWidthM.W))

  val exception = Input(Bool())
  val cause = Input(UInt(5.W))
  val epc = Input(UInt(mxLen.W))
  val evec = Output(UInt(mxLen.W))
  val loadAddr = Input(UInt(mxLen.W))

  val interrupt = Input(new Interrupt)

  val interruptPending = Output(Bool())
  val interruptCause = Output(UInt(4.W))

  val hartId = Input(UInt(hartIDWidth.W))

  val ecall = Input(EcallIE())
  val ebreak = Input(EbreakIE())
  val mret = Input(MRetIE())
  val wfi = Input(WFIIE())

  val wfiOut = Output(Bool())
  val csrWrite = Output(Bool())
}

class CSRModule(implicit p: Parameters) extends CoreModule {
  import CSR._
  import CSRInstMuxIE._

  val io = IO(new CSRIntfIO)
  val csr = new CSRList

  io.out := DontCare

//  csr.mhartid.reg := io.hartId
  csr.mhartid.write(io.hartId)

  val csrMap = Map(
    CSRAddr.mstatus -> csr.mstatus.reg,
    CSRAddr.mstatush -> csr.mstatush.reg,
    CSRAddr.misa -> csr.misa.reg,
    CSRAddr.medeleg -> csr.medeleg.reg,
    CSRAddr.mideleg -> csr.mideleg.reg,
    CSRAddr.mie -> csr.mie.reg,
    CSRAddr.mtvec -> csr.mtvec.reg,
    CSRAddr.mvendorid -> csr.mvendorid.reg,
    CSRAddr.marchid -> csr.marchid.reg,
    CSRAddr.mimpid -> csr.mimpid.reg,
    CSRAddr.mhartid -> csr.mhartid.reg,
    CSRAddr.mscratch -> csr.mscratch.reg,
    CSRAddr.mepc -> csr.mepc.reg,
    CSRAddr.mcause -> csr.mcause.reg,
    CSRAddr.mtval -> csr.mtval.reg,
    CSRAddr.mip -> csr.mip.reg,
    CSRAddr.mconfigptr -> csr.mconfigptr.reg,
    CSRAddr.menvcfg -> csr.menvcfg.reg,
    CSRAddr.menvcfgh -> csr.menvcfgh.reg,
    CSRAddr.mseccfg -> csr.mseccfg.reg,
  )

  val csrAddr = csrMap map {case (k, v) => k -> (io.ctrl.addr === k.U)}

//  printf("csrAddr = %c", csrAddr.mkString)
//  printf(cf"${csrAddr.getOrElse(768)}\n")
  val a = for ((k, v) <- csrMap) yield (k)
  val b = for ((k, v) <- csrMap) yield (csrAddr(k) -> v)
//  val c = csrAddr.getOrElse(k)
  for ((k, v) <- csrMap) {
      val c = csrAddr.getOrElse(k, false)
//    printf(cf"$csrAddr\n")
//    printf(cf"key: $c\n")
//    printf(cf"value: $v\n")
  }
  io.rd := Mux1H(for ((k, v) <- csrMap) yield (csrAddr(k) -> v.asUInt))

  val wen = (io.ctrl.inst === RW) || (io.ctrl.inst === RS) || (io.ctrl.inst === RC)
  val wdata = Mux1H(Seq(
    (io.ctrl.inst === RW) -> io.ctrl.in,
    (io.ctrl.inst === RS) -> (io.ctrl.in | io.rd),
    (io.ctrl.inst === RC) -> (io.ctrl.in & (~io.rd).asUInt),
  ))

  printf(cf"wen: $wen\n")
  printf(cf"wdata: $wdata\n")
//  printf(cf"wdata: ${wdata.asTypeOf((new MStatus).field)}\n")
  printf(cf"csraddr.mstatus: ${csrAddr(CSRAddr.mstatus)}\n")
  printf(cf"csraddr.medeleg: ${csrAddr(CSRAddr.medeleg)}\n")

  val testreg = RegInit(0.U(32.W))

  when (wen) {
    when(csrAddr(CSRAddr.mstatus)) {
      csr.mstatus.write(wdata)
      printf(cf"mstatus reg: ${csr.mstatus.reg}\n")
    }
    when(csrAddr(CSRAddr.mstatush)) {
      csr.mstatush.write(wdata)
    }
    // when (csrAddr(CSRAddr.misa)) {}
    when(csrAddr(CSRAddr.medeleg)) {
      csr.medeleg.write(wdata)
      printf(cf"medeleg reg: ${csr.medeleg.reg}\n")
    }
    when(csrAddr(CSRAddr.mideleg)) {
      csr.mideleg.write(wdata)
    }
    when(csrAddr(CSRAddr.mie)) {
      csr.mie.write(wdata)
    }
    when(csrAddr(CSRAddr.mtvec)) {
      csr.mtvec.write(wdata)
    }
    // when (csrAddr(CSRAddr.mvendorid)) {}
    // when (csrAddr(CSRAddr.marchid)) {}
    // when (csrAddr(CSRAddr.mimpid)) {}
    // when (csrAddr(CSRAddr.mhartid)) {}
    when(csrAddr(CSRAddr.mscratch)) {
      csr.mscratch.write(wdata)
    }
    when(csrAddr(CSRAddr.mepc)) {
      if (usingCompressed) {
        csr.mepc.write(Cat(wdata(mxLen - 1, 1), 0.U(1.W)))
      }
      else {
        csr.mepc.write(Cat(wdata(mxLen - 1, 2), 0.U(2.W)))
      }
    }
    // If not bigint wrapped, compile error for overflow occurs
    when(csrAddr(CSRAddr.mcause)) {
      csr.mcause.write(wdata & ((BigInt(1) << (mxLen - 1)) | ((BigInt(1) << causeWidth) - 1)).U)
    }
    when(csrAddr(CSRAddr.mtval)) {
      csr.mtval.write(wdata)
    }
    when(csrAddr(CSRAddr.mip)) {
      csr.mip.write(wdata)
    }
    // when (csrAddr(CSRAddr.mconfigptr)) {}
    when(csrAddr(CSRAddr.menvcfg)) {
      // Support S-mode or satp.MODE is read-only zero then don't write
      csr.menvcfg.write(wdata.asTypeOf((new Envcfg).field).fiom)
    }
    // when (csrAddr(CSRAddr.menvcfgh)) {}
    // when (csrAddr(CSRAddr.mseccfg)) {}
  }

  // Exception
  val exception = io.ecall.asUInt.orR || io.ebreak.asUInt.orR || io.exception

//  io.evec := Mux(csr.mtvec.reg.mode === 0.U, csr.mtvec.reg.base, csr.mtvec.reg.base + (io.cause << 2))
  io.evec := Mux(csr.mtvec.reg === 0.U, csr.mtvec.reg.base, csr.mtvec.reg.base + (io.cause << 2))
  val tval = Mux(io.ebreak.asUInt.orR, io.epc, io.loadAddr)
  when (exception) {
    csr.mepc.reg := io.epc
    csr.mcause.reg := io.cause
    csr.mtval.reg := tval
  }

  when (io.mret.asUInt.orR) {
    io.evec := csr.mepc.reg
  }

  // Interrupt
  val mip = WireInit(0.U.asTypeOf(csr.mip.reg))
  mip.meip := io.interrupt.e
  mip.mtip := io.interrupt.t
  mip.msip := io.interrupt.s

  val mInterruptPending = (csr.mie.reg.asUInt & mip.asUInt).orR
      io.interruptPending := mInterruptPending
      io.interruptCause := Mux1H (Seq(
        (csr.mip.reg.meip && csr.mie.reg.meip) -> 11.U,
        (csr.mip.reg.mtip && csr.mie.reg.mtip) -> 7.U,
        (csr.mip.reg.msip && csr.mie.reg.msip) -> 3.U,
      ))


  // WFI
  // FIXME: CLK Gating
  val wfi = RegInit(false.B)
  when (io.wfi.asUInt.orR) {
    wfi := true.B
  }
  when (mInterruptPending) {
    wfi := false.B
  }
  io.wfiOut := wfi

  // CSR write results in stall and flush
  io.csrWrite := wen
}
