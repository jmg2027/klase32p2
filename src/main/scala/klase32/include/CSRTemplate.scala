package klase32.include

import chisel3._
import chisel3.util._
import klase32.include.config._
import klase32.include.KLASE32AbstractClass._


object CSR {
  abstract class CSRRegTemplate[T <: Bundle] {
    //    class field extends Bundle
    def field: T

    def default: UInt

    // FIXME: deal with write invalid field
    val reg: T = RegInit(field, default.asTypeOf(field))
    def write[D <: Data](wdata: D): Unit = {
      val newData = Wire(field)
      newData := wdata.asTypeOf(field)
      reg := newData
    }

    def read[D <: Data](rdata: D): Unit = {
      rdata := reg.asTypeOf(rdata)
    }
  }

  abstract class CoreCSRReg[T <: Bundle](implicit val p: Parameters) extends CSRRegTemplate[T] with HasCoreParameters

  object CSRField {
    class CSRReg(implicit val p: Parameters) extends Bundle with HasCoreParameters {
      val data = UInt(csrWidthM.W)
    }

    class Dcsr extends Bundle {
      // msb 0x4000_0403
      val xdebugver = UInt(4.W)   // R    4: external debug, 4: according to standard, 15: custom external debug, PRESET
      val rsv2      = UInt(10.W)  // 0
      val ebreakvs  = Bool()      // WARL 0: default = 0
      val ebreakvu  = Bool()      // WARL 0: default = 0 //
      val ebreakm   = Bool()      // R/W  0: exception, 1: M debug mode from EBREAK, default = 0
      val rsv1      = Bool()      // 0
      val ebreaks   = Bool()      // WARL 0: exception, 1: S debug mode from EBREAK, default = 0
      val ebreaku   = Bool()      // WARL 0: exception, 1: U debug mode from EBREAK, default = 0 //
      val stepie    = Bool()      // WARL 0: interrupt enable during debug mode, 0: disable, default = 0
      val stopcount = Bool()      // WARL 1: counter stop or not at the debug mode 0, PRESET, default = 1
      val stoptime  = Bool()      // WARL 0: timer stop or not at the debug mode 0, PRESET
      val cause    = UInt(3.W)   // R       debug cause, default = 0
                                  //      5: resethaltreq
                                  //      6: haltgroup
                                  //      3: haltreq
                                  //      2: trigger
                                  //      1: ebreak
                                  //      4: step          <= lowest priority
      val v         = Bool()      // WARL 0: from virtualization mode, default = 0
      val mprven    = Bool()      // WARL 0: ignoring mstatus value at the debug mode when this is 0, PRESET
      val nmip      = Bool()      // R    0: non-maskable interrupt pending, default = 0
      val step      = Bool()      // R/W  0: single step, default = 0
      val prv       = UInt(2.W)   // WARL 3: privlevel when entering the debug mode, default = 3
      // lsb
    }

    class MControl6 extends Bundle {
      // msb 0x68001000
      val ctrltype     = UInt(4.W)   // WARL, 6, mcontrol 6
      val dmode        = Bool()      // R,    1, debug mode only can writable
      val uncertain    = Bool()      // R/C   0, corresponds to hit0
      val hit1         = Bool()      // R,    0, not supported
      val vs           = Bool()      // R,    0, not supported
      val vu           = Bool()      // R,    0, not supported
      val hit0         = Bool()      // R/C,  0, 1 - fired
      val select       = Bool()      // WARL, 0, address, 1- data
      val rsv          = UInt(2.W)   // R,    0,
      val size         = UInt(3.W)   // WARL, 0, 2, 3 only supported
      val action       = UInt(4.W)   // WARL, 1, breakpoint exception and enter debug mode(1) supported
      val chain        = Bool()      // R,    0, not supported
      val valmatch     = UInt(4.W)   // R,    0, equal only support
      val m            = Bool()      // WARL, 0, trigger enable when in m mode
      val uncertainen  = Bool()      // R,    0, not supported
      val s            = Bool()      // WARL, 0, trigger enable when in s mode
      val u            = Bool()      // WARL, 0, trigger enable when in u mode
      val execute      = Bool()      // WARL, 0, set to enable trigger matching on instr/address
      val store        = Bool()      // WARL, 0, set to enable trigger matching on store address/data
      val load         = Bool()      // WARL, 0, set to enable trigger matching on load address/data
      // lsb
    }
    class TInfo extends Bundle {
      // msb 0x10000040
      val version   = UInt(8.W)
      val rsv1      = UInt(8.W)
      val disable   = Bool()
      val custom    = UInt(3.W)
      val rsv0      = UInt(4.W)
      val tmexttrig = Bool()
      val mcontrol6 = Bool()
      val etrigger  = Bool()
      val itrigger  = Bool()
      val icount    = Bool()
      val mcontrol  = Bool()
      val legacy    = Bool()
      val notrig    = Bool()
      // lsb
    }

    class MStatus extends Bundle {
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
      val mprv = Bool()   // 0: translation and protection bahave as normal, 1: explicit memory accesses are protected and translated
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

    class MStatush extends Bundle {
      val mbe = Bool()
      val sbe = Bool()
      val zero0 = UInt(4.W)
    }

    class MIP extends Bundle {
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
    }

    class TVec(implicit val p: Parameters) extends Bundle with HasCoreParameters {
      val base = UInt((mxLen - 2).W)
      val mode = UInt(2.W)
    }
  }

  class CSRReg(val defaultValue: UInt = 0.U)(implicit p: Parameters) extends CoreCSRReg[CSRField.CSRReg] {
    override def field = new CSRField.CSRReg
    //    class Field extends Bundle {
    //      val data = UInt(csrWidthM.W)
    //    }

    def default = defaultValue
  }

  class CSRCounter(implicit p: Parameters) extends CSRReg {
    def count(inc: UInt, inhibit: Bool) = {
      when(inhibit) {
        reg := reg
      }.otherwise {
        reg.asUInt := reg.asUInt + inc
      }
    }
  }

  class Dcsr (implicit  p: Parameters) extends CoreCSRReg[CSRField.Dcsr] {
    override def field = new CSRField.Dcsr
    def default: UInt = 0x4000_0403.U
  }

  class TInfo (implicit p: Parameters) extends CoreCSRReg[CSRField.TInfo] {
    override def field = new CSRField.TInfo
    def default: UInt = 0x1000_0040.U
    // version = 1
    // mcontrol6 = 1
  }

  class TData1 (implicit p: Parameters) extends CoreCSRReg[CSRField.MControl6] {
    override def field = new CSRField.MControl6
    def default: UInt = 0x68001000.U  // for mcontrol6 status
  }

  class MStatus (implicit p: Parameters) extends CoreCSRReg[CSRField.MStatus] {
    override def field = new CSRField.MStatus

    def default = 40.U
    //    override val reg = RegInit(field, default.asTypeOf(field))
  }

  // v1.12
  class MStatush(implicit p: Parameters) extends CoreCSRReg[CSRField.MStatush] {
    def field = new CSRField.MStatush

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
  class MIP(implicit p: Parameters) extends CoreCSRReg[CSRField.MIP] {
    override def field = new CSRField.MIP

    def default = 0.U

    //    val reg = RegInit(default.asTypeOf(field))
    //    val reg = RegInit(default.asTypeOf(chiselTypeOf(new field)))
  }

  class MIE(implicit p: Parameters) extends MIP

  class Envcfg(implicit p: Parameters) extends CoreCSRReg[CSRField.Envcfg] {
    //    val field = new Bundle {
    def field = new CSRField.Envcfg


    def default = 0.U
    //    override def reg = RegInit(default.asTypeOf(field))
    //    override val reg = RegInit(default.asTypeOf(field))
  }

  class TVec(implicit p: Parameters) extends CoreCSRReg[CSRField.TVec] {
    def field = new CSRField.TVec

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

    val cycle = freechips.rocketchip.rocket.CSRs.cycle
    val time = freechips.rocketchip.rocket.CSRs.time
    val instret = freechips.rocketchip.rocket.CSRs.instret
    val hpmcounter3 = freechips.rocketchip.rocket.CSRs.hpmcounter3
    val hpmcounter4 = freechips.rocketchip.rocket.CSRs.hpmcounter4
    val hpmcounter5 = freechips.rocketchip.rocket.CSRs.hpmcounter5
    val hpmcounter6 = freechips.rocketchip.rocket.CSRs.hpmcounter6
    val hpmcounter7 = freechips.rocketchip.rocket.CSRs.hpmcounter7
    val hpmcounter8 = freechips.rocketchip.rocket.CSRs.hpmcounter8
    val hpmcounter9 = freechips.rocketchip.rocket.CSRs.hpmcounter9
    val hpmcounter10 = freechips.rocketchip.rocket.CSRs.hpmcounter10
    val hpmcounter11 = freechips.rocketchip.rocket.CSRs.hpmcounter11
    val hpmcounter12 = freechips.rocketchip.rocket.CSRs.hpmcounter12
    val hpmcounter13 = freechips.rocketchip.rocket.CSRs.hpmcounter13
    val hpmcounter14 = freechips.rocketchip.rocket.CSRs.hpmcounter14
    val hpmcounter15 = freechips.rocketchip.rocket.CSRs.hpmcounter15
    val hpmcounter16 = freechips.rocketchip.rocket.CSRs.hpmcounter16
    val hpmcounter17 = freechips.rocketchip.rocket.CSRs.hpmcounter17
    val hpmcounter18 = freechips.rocketchip.rocket.CSRs.hpmcounter18
    val hpmcounter19 = freechips.rocketchip.rocket.CSRs.hpmcounter19
    val hpmcounter20 = freechips.rocketchip.rocket.CSRs.hpmcounter20
    val hpmcounter21 = freechips.rocketchip.rocket.CSRs.hpmcounter21
    val hpmcounter22 = freechips.rocketchip.rocket.CSRs.hpmcounter22
    val hpmcounter23 = freechips.rocketchip.rocket.CSRs.hpmcounter23
    val hpmcounter24 = freechips.rocketchip.rocket.CSRs.hpmcounter24
    val hpmcounter25 = freechips.rocketchip.rocket.CSRs.hpmcounter25
    val hpmcounter26 = freechips.rocketchip.rocket.CSRs.hpmcounter26
    val hpmcounter27 = freechips.rocketchip.rocket.CSRs.hpmcounter27
    val hpmcounter28 = freechips.rocketchip.rocket.CSRs.hpmcounter28
    val hpmcounter29 = freechips.rocketchip.rocket.CSRs.hpmcounter29
    val hpmcounter30 = freechips.rocketchip.rocket.CSRs.hpmcounter30
    val hpmcounter31 = freechips.rocketchip.rocket.CSRs.hpmcounter31

    val cycleh = freechips.rocketchip.rocket.CSRs.cycleh
    val timeh = freechips.rocketchip.rocket.CSRs.timeh
    val instreth = freechips.rocketchip.rocket.CSRs.instreth
    val hpmcounter3h = freechips.rocketchip.rocket.CSRs.hpmcounter3h
    val hpmcounter4h = freechips.rocketchip.rocket.CSRs.hpmcounter4h
    val hpmcounter5h = freechips.rocketchip.rocket.CSRs.hpmcounter5h
    val hpmcounter6h = freechips.rocketchip.rocket.CSRs.hpmcounter6h
    val hpmcounter7h = freechips.rocketchip.rocket.CSRs.hpmcounter7h
    val hpmcounter8h = freechips.rocketchip.rocket.CSRs.hpmcounter8h
    val hpmcounter9h = freechips.rocketchip.rocket.CSRs.hpmcounter9h
    val hpmcounter10h = freechips.rocketchip.rocket.CSRs.hpmcounter10h
    val hpmcounter11h = freechips.rocketchip.rocket.CSRs.hpmcounter11h
    val hpmcounter12h = freechips.rocketchip.rocket.CSRs.hpmcounter12h
    val hpmcounter13h = freechips.rocketchip.rocket.CSRs.hpmcounter13h
    val hpmcounter14h = freechips.rocketchip.rocket.CSRs.hpmcounter14h
    val hpmcounter15h = freechips.rocketchip.rocket.CSRs.hpmcounter15h
    val hpmcounter16h = freechips.rocketchip.rocket.CSRs.hpmcounter16h
    val hpmcounter17h = freechips.rocketchip.rocket.CSRs.hpmcounter17h
    val hpmcounter18h = freechips.rocketchip.rocket.CSRs.hpmcounter18h
    val hpmcounter19h = freechips.rocketchip.rocket.CSRs.hpmcounter19h
    val hpmcounter20h = freechips.rocketchip.rocket.CSRs.hpmcounter20h
    val hpmcounter21h = freechips.rocketchip.rocket.CSRs.hpmcounter21h
    val hpmcounter22h = freechips.rocketchip.rocket.CSRs.hpmcounter22h
    val hpmcounter23h = freechips.rocketchip.rocket.CSRs.hpmcounter23h
    val hpmcounter24h = freechips.rocketchip.rocket.CSRs.hpmcounter24h
    val hpmcounter25h = freechips.rocketchip.rocket.CSRs.hpmcounter25h
    val hpmcounter26h = freechips.rocketchip.rocket.CSRs.hpmcounter26h
    val hpmcounter27h = freechips.rocketchip.rocket.CSRs.hpmcounter27h
    val hpmcounter28h = freechips.rocketchip.rocket.CSRs.hpmcounter28h
    val hpmcounter29h = freechips.rocketchip.rocket.CSRs.hpmcounter29h
    val hpmcounter30h = freechips.rocketchip.rocket.CSRs.hpmcounter30h
    val hpmcounter31h = freechips.rocketchip.rocket.CSRs.hpmcounter31h

    val mcountinhibit = freechips.rocketchip.rocket.CSRs.mcountinhibit

    val mcycle = freechips.rocketchip.rocket.CSRs.mcycle
    val minstret = freechips.rocketchip.rocket.CSRs.minstret
    val mhpmcounter3 = freechips.rocketchip.rocket.CSRs.mhpmcounter3
    val mhpmcounter4 = freechips.rocketchip.rocket.CSRs.mhpmcounter4
    val mhpmcounter5 = freechips.rocketchip.rocket.CSRs.mhpmcounter5
    val mhpmcounter6 = freechips.rocketchip.rocket.CSRs.mhpmcounter6
    val mhpmcounter7 = freechips.rocketchip.rocket.CSRs.mhpmcounter7
    val mhpmcounter8 = freechips.rocketchip.rocket.CSRs.mhpmcounter8
    val mhpmcounter9 = freechips.rocketchip.rocket.CSRs.mhpmcounter9
    val mhpmcounter10 = freechips.rocketchip.rocket.CSRs.mhpmcounter10
    val mhpmcounter11 = freechips.rocketchip.rocket.CSRs.mhpmcounter11
    val mhpmcounter12 = freechips.rocketchip.rocket.CSRs.mhpmcounter12
    val mhpmcounter13 = freechips.rocketchip.rocket.CSRs.mhpmcounter13
    val mhpmcounter14 = freechips.rocketchip.rocket.CSRs.mhpmcounter14
    val mhpmcounter15 = freechips.rocketchip.rocket.CSRs.mhpmcounter15
    val mhpmcounter16 = freechips.rocketchip.rocket.CSRs.mhpmcounter16
    val mhpmcounter17 = freechips.rocketchip.rocket.CSRs.mhpmcounter17
    val mhpmcounter18 = freechips.rocketchip.rocket.CSRs.mhpmcounter18
    val mhpmcounter19 = freechips.rocketchip.rocket.CSRs.mhpmcounter19
    val mhpmcounter20 = freechips.rocketchip.rocket.CSRs.mhpmcounter20
    val mhpmcounter21 = freechips.rocketchip.rocket.CSRs.mhpmcounter21
    val mhpmcounter22 = freechips.rocketchip.rocket.CSRs.mhpmcounter22
    val mhpmcounter23 = freechips.rocketchip.rocket.CSRs.mhpmcounter23
    val mhpmcounter24 = freechips.rocketchip.rocket.CSRs.mhpmcounter24
    val mhpmcounter25 = freechips.rocketchip.rocket.CSRs.mhpmcounter25
    val mhpmcounter26 = freechips.rocketchip.rocket.CSRs.mhpmcounter26
    val mhpmcounter27 = freechips.rocketchip.rocket.CSRs.mhpmcounter27
    val mhpmcounter28 = freechips.rocketchip.rocket.CSRs.mhpmcounter28
    val mhpmcounter29 = freechips.rocketchip.rocket.CSRs.mhpmcounter29
    val mhpmcounter30 = freechips.rocketchip.rocket.CSRs.mhpmcounter30
    val mhpmcounter31 = freechips.rocketchip.rocket.CSRs.mhpmcounter31

    val mcycleh = freechips.rocketchip.rocket.CSRs.mcycleh
    val minstreth = freechips.rocketchip.rocket.CSRs.minstreth
    val mhpmcounter3h = freechips.rocketchip.rocket.CSRs.mhpmcounter3h
    val mhpmcounter4h = freechips.rocketchip.rocket.CSRs.mhpmcounter4h
    val mhpmcounter5h = freechips.rocketchip.rocket.CSRs.mhpmcounter5h
    val mhpmcounter6h = freechips.rocketchip.rocket.CSRs.mhpmcounter6h
    val mhpmcounter7h = freechips.rocketchip.rocket.CSRs.mhpmcounter7h
    val mhpmcounter8h = freechips.rocketchip.rocket.CSRs.mhpmcounter8h
    val mhpmcounter9h = freechips.rocketchip.rocket.CSRs.mhpmcounter9h
    val mhpmcounter10h = freechips.rocketchip.rocket.CSRs.mhpmcounter10h
    val mhpmcounter11h = freechips.rocketchip.rocket.CSRs.mhpmcounter11h
    val mhpmcounter12h = freechips.rocketchip.rocket.CSRs.mhpmcounter12h
    val mhpmcounter13h = freechips.rocketchip.rocket.CSRs.mhpmcounter13h
    val mhpmcounter14h = freechips.rocketchip.rocket.CSRs.mhpmcounter14h
    val mhpmcounter15h = freechips.rocketchip.rocket.CSRs.mhpmcounter15h
    val mhpmcounter16h = freechips.rocketchip.rocket.CSRs.mhpmcounter16h
    val mhpmcounter17h = freechips.rocketchip.rocket.CSRs.mhpmcounter17h
    val mhpmcounter18h = freechips.rocketchip.rocket.CSRs.mhpmcounter18h
    val mhpmcounter19h = freechips.rocketchip.rocket.CSRs.mhpmcounter19h
    val mhpmcounter20h = freechips.rocketchip.rocket.CSRs.mhpmcounter20h
    val mhpmcounter21h = freechips.rocketchip.rocket.CSRs.mhpmcounter21h
    val mhpmcounter22h = freechips.rocketchip.rocket.CSRs.mhpmcounter22h
    val mhpmcounter23h = freechips.rocketchip.rocket.CSRs.mhpmcounter23h
    val mhpmcounter24h = freechips.rocketchip.rocket.CSRs.mhpmcounter24h
    val mhpmcounter25h = freechips.rocketchip.rocket.CSRs.mhpmcounter25h
    val mhpmcounter26h = freechips.rocketchip.rocket.CSRs.mhpmcounter26h
    val mhpmcounter27h = freechips.rocketchip.rocket.CSRs.mhpmcounter27h
    val mhpmcounter28h = freechips.rocketchip.rocket.CSRs.mhpmcounter28h
    val mhpmcounter29h = freechips.rocketchip.rocket.CSRs.mhpmcounter29h
    val mhpmcounter30h = freechips.rocketchip.rocket.CSRs.mhpmcounter30h
    val mhpmcounter31h = freechips.rocketchip.rocket.CSRs.mhpmcounter31h

    val mhpmevent3 = freechips.rocketchip.rocket.CSRs.mhpmevent3
    val mhpmevent4 = freechips.rocketchip.rocket.CSRs.mhpmevent4
    val mhpmevent5 = freechips.rocketchip.rocket.CSRs.mhpmevent5
    val mhpmevent6 = freechips.rocketchip.rocket.CSRs.mhpmevent6
    val mhpmevent7 = freechips.rocketchip.rocket.CSRs.mhpmevent7
    val mhpmevent8 = freechips.rocketchip.rocket.CSRs.mhpmevent8
    val mhpmevent9 = freechips.rocketchip.rocket.CSRs.mhpmevent9
    val mhpmevent10 = freechips.rocketchip.rocket.CSRs.mhpmevent10
    val mhpmevent11 = freechips.rocketchip.rocket.CSRs.mhpmevent11
    val mhpmevent12 = freechips.rocketchip.rocket.CSRs.mhpmevent12
    val mhpmevent13 = freechips.rocketchip.rocket.CSRs.mhpmevent13
    val mhpmevent14 = freechips.rocketchip.rocket.CSRs.mhpmevent14
    val mhpmevent15 = freechips.rocketchip.rocket.CSRs.mhpmevent15
    val mhpmevent16 = freechips.rocketchip.rocket.CSRs.mhpmevent16
    val mhpmevent17 = freechips.rocketchip.rocket.CSRs.mhpmevent17
    val mhpmevent18 = freechips.rocketchip.rocket.CSRs.mhpmevent18
    val mhpmevent19 = freechips.rocketchip.rocket.CSRs.mhpmevent19
    val mhpmevent20 = freechips.rocketchip.rocket.CSRs.mhpmevent20
    val mhpmevent21 = freechips.rocketchip.rocket.CSRs.mhpmevent21
    val mhpmevent22 = freechips.rocketchip.rocket.CSRs.mhpmevent22
    val mhpmevent23 = freechips.rocketchip.rocket.CSRs.mhpmevent23
    val mhpmevent24 = freechips.rocketchip.rocket.CSRs.mhpmevent24
    val mhpmevent25 = freechips.rocketchip.rocket.CSRs.mhpmevent25
    val mhpmevent26 = freechips.rocketchip.rocket.CSRs.mhpmevent26
    val mhpmevent27 = freechips.rocketchip.rocket.CSRs.mhpmevent27
    val mhpmevent28 = freechips.rocketchip.rocket.CSRs.mhpmevent28
    val mhpmevent29 = freechips.rocketchip.rocket.CSRs.mhpmevent29
    val mhpmevent30 = freechips.rocketchip.rocket.CSRs.mhpmevent30
    val mhpmevent31 = freechips.rocketchip.rocket.CSRs.mhpmevent31

    // for debugger
    val dcsr        = freechips.rocketchip.rocket.CSRs.dcsr
    val dpc         = freechips.rocketchip.rocket.CSRs.dpc
    val dscratch0   = freechips.rocketchip.rocket.CSRs.dscratch0
    val dscratch1   = freechips.rocketchip.rocket.CSRs.dscratch1
    val tselect     = freechips.rocketchip.rocket.CSRs.tselect
    val tdata1      = freechips.rocketchip.rocket.CSRs.tdata1
    val tdata2      = freechips.rocketchip.rocket.CSRs.tdata2
    val tdata3      = freechips.rocketchip.rocket.CSRs.tdata3
    val tinfo       = freechips.rocketchip.rocket.CSRs.tinfo
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

    val mcounterinhibit = new CSRReg
//    val mcycle = new CSRReg
    val mcycle = new CSRCounter
    val minstret = new CSRCounter
    val mcycleh = new CSRCounter
    val minstreth = new CSRCounter

    val mhpmcounter = {
      for (i <- 3 until 32) yield new CSRCounter
    }
    val mhpmevent = {
      for (i <- 3 until 32) yield new CSRCounter
    }
    val mhpmcounterh = {
      for (i <- 3 until 32) yield new CSRCounter
    }
    val mhpmeventh = {
      for (i <- 3 until 32) yield new CSRCounter
    }
    // for debugger
    val dcsr = new Dcsr
    val dpc = new CSRReg
    val dscratch0 = new CSRReg
    val dscratch1 = new CSRReg
    val tselect = new CSRReg
    val tdata1 = new TData1
    val tdata2 = new CSRReg
    val tdata3 = new CSRReg
    val tinfo = new TInfo
  }
}
