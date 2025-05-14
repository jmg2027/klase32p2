package klase32.functionalunit

import chisel3._
import freechips.rocketchip.rocket.Causes
import chisel3.util._
import klase32.include.CSR.CSRField.MControl6
import klase32.include.KLASE32AbstractClass._
import klase32.include.ControlSignal._
import klase32.include.Interrupt
import klase32.include.config._
import klase32.include.Constants
import klase32.include.param.KLASE32ParamKey
import klase32.common.FunctionUnitIO

// Bundles
class CSRReq(implicit p: Parameters) extends CoreBundle with HasCoreParameters {
  // CSR control and R/W behavior
  val ctrl = new Bundle {
    val in = UInt(csrWidthM.W)
    val inst = CSRControl()
    val addr = UInt(12.W)
  }
  val rd = UInt(regIdWidth.W)
}

class CSRResp(implicit p: Parameters) extends CoreBundle with HasCoreParameters {
  val wbData = UInt(csrWidthM.W)  // writeback data from CSR to Register
  val csrWriteFlush = Bool()      // transfer to Flush.IE
  val rd = UInt(regIdWidth.W)
}

class CSRTrapSource(implicit p: Parameters) extends CoreBundle with HasCoreParameters {
  class TrapSource (implicit p: Parameters) extends Bundle {
    val exception = Bool()
    val cause = UInt(Causes.all.length.W)
    val pc = UInt(mxLen.W)
  }
  val ie = new TrapSource
  val me = new TrapSource
}


class CSRDebug(implicit p: Parameters) extends CoreBundle with HasCoreParameters {
//  val dbgFire = Bool() // broadcasting debug fire
//  val regDbgMode = Bool() // broadcasting debug mode
  val fire = Bool() // broadcasting debug fire
  val mode = Bool() // broadcasting debug mode
  val dpc = UInt(mxLen.W)         // broadcasting dpc
}

class CSRTriggerSource(implicit p: Parameters) extends CoreBundle with HasCoreParameters {
  val k = p(KLASE32ParamKey)
  val pc = UInt(mxLen.W) // Normally is ie stage pc
  val instruction = UInt(32.W)
  val loadAddr = UInt(k.vaddrBits.W) // valid load address
  val storeAddr = UInt(k.vaddrBits.W) // valid store address
  val loadData = UInt(k.dataWidth.W) // load data
  val storeData = UInt(k.dataWidth.W) // store data
}

class CSRTriggerFire(implicit p: Parameters) extends CoreBundle with HasCoreParameters {
  val debugMode = Bool() // broadcast trigger dmode
  val loadData = Bool() // broadcast load trigger not to block writeback
  val breakPoint = new Bundle {
    val exe = Bool()
    val load = Bool()
    val store = Bool()
  }
}

class CSRStatus(implicit p: Parameters) extends CoreBundle with HasCoreParameters {
  val mcause = UInt(mxLen.W)
  val mstatus = UInt(mxLen.W)
  val mstatush = UInt(mxLen.W)
  val mepc = UInt(mxLen.W)
}

class CSRSystemInstruction(implicit p: Parameters) extends CoreBundle with HasCoreParameters {
  val ecall = EcallIE() // ecall enable from decoder
  val ebreak = EbreakIE() // ebreak enable from decoder
}

class CSRReturnInstruction(implicit p: Parameters) extends CoreBundle with HasCoreParameters {
  val mret = MRetIE() // mret enalbe from decoder
  // for debugger
  val dret = DRetIE() // dret enable from decoder
}

class CSRInterruptPending(implicit p: Parameters) extends CoreBundle with HasCoreParameters {
//  val interruptPending = Output(Bool()) // interrupt pending from mstatus
//  val interruptCause = Output(UInt(4.W)) // interrupt cause from mip and mie
  val pending = Bool() // interrupt pending from mstatus
  val cause = UInt(4.W) // interrupt cause from mip and mie
}

class CSRResetHartRequest(implicit p: Parameters) extends CoreBundle with HasCoreParameters {
//  val regBooting = Input(Bool())          // for ResetHartRequest
  val booting = Bool()          // for ResetHartRequest
  val bootAddr = UInt(mxLen.W)     // for ResetHartRequest
}

// I/O
class CSRSideband(implicit p: Parameters)
  extends CoreBundle
    with HasCoreParameters {
  val k = p(KLASE32ParamKey)

  val hartId = Input(UInt(hartIDWidth.W))

  val ie_inst_valid = Input(Bool())

  val trap = Input(new CSRTrapSource())
  val interrupt = Input(new Interrupt)    // interrupt from sw, timer, external
  val interruptPending = Output(new CSRInterruptPending())

  val debug = Output(new CSRDebug())
  val trigSrc = Input(new CSRTriggerSource())
  val trigFire = Output(new CSRTriggerFire())

  val csr = Output(new CSRStatus())

  val sys = Input(new CSRSystemInstruction())
  val ret = Input(new CSRReturnInstruction())

  val evec = Output(UInt(mxLen.W)) // vector address of exception

  val resetReq = Input(new CSRResetHartRequest())

  val wfi = Input(WFIIE()) // wfi enable from decoder
  val wfiOut = Output(Bool()) // for clock gating
}

// Dummy wrapper for function unit template
class CSR(implicit p: Parameters) extends CoreModule
with FunctionUnitIO[CSRReq, CSRResp] {
  val req = IO(Flipped(Decoupled(new CSRReq)))
  val resp = IO(Decoupled(new CSRResp))

  val io = IO(new CSRSideband())

  private val core = Module(new CSRCore)

  core.req <> req.bits
  resp.bits <> core.resp
  io <> core.io

  // Handshake
  req.ready := true.B
  resp.valid := req.valid // No delay
}

class CSRCore(implicit p: Parameters) extends CoreModule {
  import klase32.include.CSR._
  import CSRControl._

  val req = IO(Input(new CSRReq))
  val resp = IO(Output(new CSRResp))
  val io = IO(new CSRSideband())

  val csr = new CSRList
  val exception = WireInit(false.B)

  csr.mhartid.write(io.hartId)

  var csrMap = Map(
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
    // for debugger
    CSRAddr.dcsr -> csr.dcsr.reg,
    CSRAddr.dpc -> csr.dpc.reg,
    CSRAddr.dscratch0 -> csr.dscratch0.reg,
    CSRAddr.dscratch1 -> csr.dscratch1.reg,
    CSRAddr.tselect -> csr.tselect.reg,
    CSRAddr.tdata1 -> csr.tdata1.reg,
    CSRAddr.tdata2 -> csr.tdata2.reg,
    CSRAddr.tdata3 -> csr.tdata3.reg,
    CSRAddr.tinfo -> csr.tinfo.reg,
  )

  // HPM Counters
  csrMap += CSRAddr.mcountinhibit -> csr.mcounterinhibit.reg

  csrMap += CSRAddr.cycle -> csr.mcycle.reg
  csrMap += CSRAddr.instret -> csr.minstret.reg
  csrMap += CSRAddr.mcycle -> csr.mcycle.reg
  csrMap += CSRAddr.minstret -> csr.minstret.reg
  if (xLen == 32) {
    csrMap += CSRAddr.cycleh -> csr.mcycleh.reg
    csrMap += CSRAddr.instreth -> csr.minstreth.reg
    csrMap += CSRAddr.mcycleh -> csr.mcycleh.reg
    csrMap += CSRAddr.minstreth -> csr.minstreth.reg
  }

  for (((event, counter), i) <- (csr.mhpmevent zip csr.mhpmcounter).zipWithIndex) {
    csrMap += (i + CSRAddr.mhpmevent3) -> event.reg
    csrMap += (i + CSRAddr.mhpmcounter3) -> counter.reg
    csrMap += (i + CSRAddr.hpmcounter3) -> counter.reg
    // Platform dependent implementation
    counter.count(event.reg.asUInt, csr.mcounterinhibit.reg.asUInt(i+3))
  }

  if (xLen == 32) {
    for ((counter, i) <- csr.mhpmcounterh.zipWithIndex) {
      csrMap += (i + CSRAddr.mhpmcounter3h) -> (counter.reg)
      csrMap += (i + CSRAddr.hpmcounter3h) -> (counter.reg)
    }
  }

  // Essential Counters
  csr.mcycle.count(!io.wfiOut, csr.mcounterinhibit.reg.asUInt(0))
  csr.minstret.count(io.ie_inst_valid && !exception, csr.mcounterinhibit.reg.asUInt(2))
  // When lower 32-bit counter overflows, high 32-bit register increases
  if (xLen == 32) {
    csr.mcycleh.count(csr.mcycle.reg.asUInt(xLen - 1), false.B)
    csr.minstreth.count(csr.minstret.reg.asUInt(xLen - 1), false.B)
  }

  val csrAddr = csrMap map {case (k, v) => k -> (req.ctrl.addr === k.U)}

  val a = for ((k, v) <- csrMap) yield (k)
  val b = for ((k, v) <- csrMap) yield (csrAddr(k) -> v)
  resp.wbData := Mux1H(for ((k, v) <- csrMap) yield (csrAddr(k) -> v.asUInt))

  // FIXME: CSR illegal instruction check
  val wen = (req.ctrl.inst === RW) || (req.ctrl.inst === RS) || (req.ctrl.inst === RC)
  val wdata = Mux1H(Seq(
    (req.ctrl.inst === RW) -> req.ctrl.in,
    (req.ctrl.inst === RS) -> (req.ctrl.in | resp.wbData),
    (req.ctrl.inst === RC) -> ((req.ctrl.in | resp.wbData) & (~req.ctrl.in).asUInt),
  ))

  when (wen) {
    when(csrAddr(CSRAddr.mstatus)) {
      csr.mstatus.write(wdata)
    }
    when(csrAddr(CSRAddr.mstatush)) {
      csr.mstatush.write(wdata)
    }
    // when (csrAddr(CSRAddr.misa)) {}
    when(csrAddr(CSRAddr.medeleg)) {
      csr.medeleg.write(wdata)
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

    // for Debugger
    when(csrAddr(CSRAddr.dscratch0)) {
      csr.dscratch0.write(wdata)
    }
    when(csrAddr(CSRAddr.dscratch1)) {
      csr.dscratch1.write(wdata)
    }
    when(csrAddr(CSRAddr.dcsr)) {
      csr.dcsr.write(wdata)
    }
    when(csrAddr(CSRAddr.dpc)) {
      if (usingCompressed) {
        csr.dpc.write(Cat(wdata(mxLen - 1, 1), 0.U(1.W)))
      }
      else {
        csr.dpc.write(Cat(wdata(mxLen - 1, 2), 0.U(2.W)))
      }
    }
    when(csrAddr(CSRAddr.tdata1)) {
      csr.tdata1.write(wdata)
    }
    when(csrAddr(CSRAddr.tdata2)) {
      csr.tdata2.write(wdata)
    }
    when(csrAddr(CSRAddr.tdata3)) {
      csr.tdata3.write(wdata)
    }
    when(csrAddr(CSRAddr.tinfo)) {
      csr.tinfo.write(wdata)
    }
    when(csrAddr(CSRAddr.tselect)) {
      csr.tselect.write(wdata)
    }
  }

  val priv = RegInit(Constants.Privilege.MMode)

  // for debugger
  val trigFire = WireInit(false.B)
  val mcontrol6 = Wire(new MControl6)
  val trigEn = WireInit(false.B)
  csr.tdata1.read(mcontrol6)
  //trigEn := (mcontrol6.ctrltype === Constants.DebugType.MControl6).asBool && (mcontrol6.valmatch === 0.U).asBool && (mcontrol6.action === Constants.ActionType.DMode).asBool && (
//    (mcontrol6.u && (priv === Constants.Privilege.UMode).asBool) || (mcontrol6.s && (priv === Constants.Privilege.SMode).asBool) || (mcontrol6.m && (priv === Constants.Privilege.MMode).asBool)
//    ).asBool
  trigEn := (mcontrol6.ctrltype === Constants.DebugType.MControl6).asBool && (mcontrol6.valmatch === 0.U).asBool && (
    (mcontrol6.u && (priv === Constants.Privilege.UMode).asBool) || (mcontrol6.s && (priv === Constants.Privilege.SMode).asBool) || (mcontrol6.m && (priv === Constants.Privilege.MMode).asBool)
    ).asBool

  val trigExeAddrEn = mcontrol6.execute && !mcontrol6.select
  val trigExeDataEn = mcontrol6.execute && mcontrol6.select
  val trigLdAddrEn = mcontrol6.load && !mcontrol6.select
  val trigLdDataEn = mcontrol6.load && mcontrol6.select
  val trigStAddrEn = mcontrol6.store && !mcontrol6.select
  val trigStDataEn = mcontrol6.store && mcontrol6.select

  val trigFireExe = WireInit(false.B)
  val trigFireLdAddr = WireInit(false.B)
  val trigFireLdData = WireInit(false.B)
  val trigFireSt = WireInit(false.B)
  val regDebugMode = RegInit(0.B)
  when(!regDebugMode && trigEn) {
    when(trigExeDataEn) {
//      trigFireExe := csr.tdata2.reg.data === io.instr && (io.hpmIntf.ie_inst_valid)    // Only for valid instruction
      // FIXME: How to fix combinational loop?
      trigFireExe := csr.tdata2.reg.data === io.trigSrc.instruction    // Only for valid instruction
    }.elsewhen(trigExeAddrEn) {
//      trigFireExe := csr.tdata2.reg.data === io.iePC && (io.hpmIntf.ie_inst_valid)     // Only for valid instruction
      trigFireExe := csr.tdata2.reg.data === io.trigSrc.pc     // Only for valid instruction
    }
    when(trigLdAddrEn) {
      trigFireLdAddr := csr.tdata2.reg.data === io.trigSrc.loadAddr
    }
    when(trigLdDataEn) {
      trigFireLdData := csr.tdata2.reg.data === io.trigSrc.loadData
    }
    when(trigStAddrEn) {
      trigFireSt := csr.tdata2.reg.data === io.trigSrc.storeAddr
    }.elsewhen(trigStDataEn) {
      trigFireSt := csr.tdata2.reg.data === io.trigSrc.storeData
    }
  }

  trigFire := trigFireExe || trigFireSt || trigFireLdAddr || trigFireLdData
  io.trigFire.breakPoint.exe := (mcontrol6.action === Constants.ActionType.Breakpoint) && trigFireExe
  io.trigFire.breakPoint.load  := (mcontrol6.action === Constants.ActionType.Breakpoint) && (trigFireLdAddr || trigFireLdData)
  io.trigFire.breakPoint.store  := (mcontrol6.action === Constants.ActionType.Breakpoint) && trigFireSt
  io.trigFire.debugMode := (mcontrol6.action === Constants.ActionType.DMode) && trigFire
  io.trigFire.loadData := trigFireLdData

  val ebreakToDebugMode = WireInit(false.B)
  ebreakToDebugMode := io.sys.ebreak.asUInt.orR && ((csr.dcsr.reg.ebreaks && (priv===Constants.Privilege.SMode)) ||
    (csr.dcsr.reg.ebreaku && (priv===Constants.Privilege.UMode)) || (csr.dcsr.reg.ebreakm && (priv===Constants.Privilege.MMode)))
  exception := io.sys.ecall.asUInt.orR ||
    (io.sys.ebreak.asUInt.orR && !ebreakToDebugMode) ||
    io.trap.ie.exception ||
    io.trap.me.exception

  val debugFire = WireInit(0.B)
  val singleStep = io.ie_inst_valid && !regDebugMode && csr.dcsr.reg.step
  // After single step, PC must be fired by debugFire!
  debugFire := (io.interrupt.d && !regDebugMode) || singleStep || io.trigFire.debugMode
  regDebugMode := Mux(regDebugMode && io.ret.dret.asUInt.orR, false.B, debugFire || regDebugMode)
  io.debug.fire := debugFire
  io.debug.mode := regDebugMode

  // Exception
  // tval
  // Overview: breakpoint, address-misaligned, access-fault, or
  // page-fault exception occurs on an instruction prefetchReq, load, or store --> faulting virtual address
  // 1. misaligned load or store causes an access-fault or page-fault exception --> virtual address of the portion of the access
  // 2. instruction access-fault or page-fault exception occurs on a system
  // with variable-length instructions --> virtual address of the portion of the instruction
  // 3. illegal instruction exception --> faulting instruction bits
  val tval = WireDefault(0.U(xLen.W))
  when(
    io.trap.ie.cause === Causes.breakpoint.U ||
      io.trap.ie.cause === Causes.machine_ecall.U
  ) {
    tval := io.trap.ie.pc
  }.elsewhen(io.trap.ie.cause === Causes.illegal_instruction.U) {
    tval := io.trigSrc.instruction
  }.elsewhen(
    io.trap.ie.cause === Causes.misaligned_fetch.U ||
      io.trap.ie.cause === Causes.fetch_page_fault.U ||
      io.trap.ie.cause === Causes.fetch_guest_page_fault.U ||
      io.trap.ie.cause === Causes.fetch_access.U ||
      io.trap.ie.cause === Causes.misaligned_store.U ||
      io.trap.ie.cause === Causes.store_page_fault.U ||
      io.trap.ie.cause === Causes.store_guest_page_fault.U ||
      io.trap.ie.cause === Causes.store_access.U
  ) {
    // FIXME: Implement
    tval := 0.U
  }.elsewhen(
    io.trap.me.cause === Causes.misaligned_load.U ||
      io.trap.me.cause === Causes.load_page_fault.U ||
      io.trap.me.cause === Causes.load_guest_page_fault.U ||
      io.trap.me.cause === Causes.load_access.U
  ) {
    tval := io.trigSrc.loadAddr
  }.otherwise {
    tval := 0.U
  }
  // FIXME: TVAL mapping information



  // Interrupt
  val mip = WireInit(0.U.asTypeOf(csr.mip.reg))
  mip.meip := io.interrupt.e
  mip.mtip := io.interrupt.t
  mip.msip := io.interrupt.s
  mip.debug := io.interrupt.d

  // FIXME: prevent mip write
  csr.mip.reg.meip := mip.meip
  csr.mip.reg.mtip := mip.mtip
  csr.mip.reg.msip := mip.msip
  csr.mip.reg.debug := mip.debug

  // FIXME: Value match to string
  // mstatus.mie shows interrupts are to be taken or not
  // When not taken, interrupt will resume its original program flow by local interrupt enables
  // When taken, interrupt will jump to ISR
  val mInterruptPending = (csr.mie.reg.asUInt & mip.asUInt).orR
  val mInterruptToBeTaken = mInterruptPending && csr.mstatus.reg.mie
  io.interruptPending.pending := mInterruptToBeTaken
  io.interruptPending.cause := Mux1H (Seq(
    (mip.debug && csr.mie.reg.debug) -> 14.U,
    (mip.meip && csr.mie.reg.meip) -> 11.U,
    (mip.mtip && csr.mie.reg.mtip) -> 7.U,
    (mip.msip && csr.mie.reg.msip) -> 3.U,
  ))

  // CSR control when interrupt/exception occurs
  io.evec := 0.U
//  when (mInterruptPending || exception) {
  when (exception) {
    csr.mstatus.reg.mpie := csr.mstatus.reg.mie
    csr.mstatus.reg.mpp := priv
    csr.mstatus.reg.mie := false.B
    when(io.trap.me.exception) {
      csr.mepc.write(io.trap.me.pc)
      csr.mcause.write(io.trap.me.cause)
    }.otherwise {
      csr.mepc.write(io.trap.ie.pc)
      csr.mcause.write(io.trap.ie.cause)
    }
    csr.mtval.write(tval)
    io.evec := Mux1H(Seq(
      (csr.mtvec.reg.mode === 0.U) -> (csr.mtvec.reg.base << 2.U),
      (csr.mtvec.reg.mode === 1.U) -> ((csr.mtvec.reg.base << 2.U).asUInt + (io.trap.ie.cause << 2.U).asUInt)
    ))
  }.elsewhen (io.ret.mret.asUInt.orR) {
    csr.mstatus.reg.mie := csr.mstatus.reg.mpie
    csr.mstatus.reg.mpie := true.B
    csr.mepc.read(io.evec)
    priv := csr.mstatus.reg.mpp
  }

//  when(mInterruptPending || exception) {
  when(exception) {
    priv := Constants.Privilege.MMode
  }.elsewhen(debugFire) {
    priv := Constants.Privilege.MMode
  }.elsewhen(io.ret.mret.asUInt.orR) {
    priv := csr.mstatus.reg.mpp
  }.elsewhen(io.ret.dret.asUInt.orR) {
    priv := csr.dcsr.reg.prv
  }


  // dcsr update for debugger

  csr.dcsr.reg.prv := Mux(debugFire, priv, csr.dcsr.reg.prv)
  val dbgCause = WireInit(0.U(3.W))
  // Priority of dcsr.cause
  // resethaltreq -> haltgroup -> haltreq -> trigger -> ebreak -> step
  dbgCause := PriorityMux(Seq(
    (!singleStep && debugFire && io.resetReq.booting) -> Constants.DebugCause.ResetHartReq,
    (!singleStep && debugFire && !io.resetReq.booting) -> Constants.DebugCause.HartReq,
    trigFire -> Constants.DebugCause.Trigger,
    ebreakToDebugMode -> Constants.DebugCause.EBreak,
    csr.dcsr.reg.step -> Constants.DebugCause.SingleStep,
  ))
  when(debugFire) {
    csr.dcsr.reg.cause := dbgCause
  }

  // dpc update for debugger
  val dpcComb = WireInit(0.U(xLen.W))
  dpcComb := Mux1H(Seq(
    //    (csr.dcsr.reg.cause === Constants.DebugCause.ResetHartReq) -> io.bootAddr,
    //    ((csr.dcsr.reg.cause === Constants.DebugCause.Trigger) && trigLdDataEn) -> (io.iePC + Mux(io.iePC(1), 2.U, 4.U)),
    //    (csr.dcsr.reg.cause === Constants.DebugCause.SingleStep) -> (io.iePC + Mux(io.iePC(1), 2.U, 4.U)),
    //    (csr.dcsr.reg.cause === Constants.DebugCause.EBreak) -> io.iePC,
    //    (csr.dcsr.reg.cause === Constants.DebugCause.HartReq) -> io.iePC,
    //    (csr.dcsr.reg.cause === Constants.DebugCause.GroupHartReq) -> io.iePC,
    (dbgCause === Constants.DebugCause.ResetHartReq) -> io.resetReq.bootAddr,
    ((dbgCause === Constants.DebugCause.Trigger) && trigLdDataEn) -> (io.trigSrc.pc + Mux(io.trigSrc.pc(1), 2.U, 4.U)),
    (dbgCause === Constants.DebugCause.SingleStep) -> (io.trigSrc.pc + Mux(io.trigSrc.pc(1), 2.U, 4.U)),
    (dbgCause === Constants.DebugCause.EBreak) -> io.trigSrc.pc,
    (dbgCause === Constants.DebugCause.HartReq) -> io.trigSrc.pc,
    (dbgCause === Constants.DebugCause.GroupHartReq) -> io.trigSrc.pc,
  ))
  when(debugFire) {
    csr.dpc.reg.data := dpcComb
  }
  /*
  csr.dpc.reg.data := Mux1H(Seq(
    (csr.dcsr.reg.cause === Constants.DebugCause.ResetHartReq) -> bootAddrParam.U,
    ((csr.dcsr.reg.cause === Constants.DebugCause.Trigger) && trigLdDataEn) -> (io.iePC + Mux(io.iePC(1), 2.U, 4.U)),
    (csr.dcsr.reg.cause === Constants.DebugCause.SingleStep) -> (io.iePC + Mux(io.iePC(1), 2.U, 4.U)),
    (csr.dcsr.reg.cause === Constants.DebugCause.EBreak) -> io.iePC,
    (csr.dcsr.reg.cause === Constants.DebugCause.HartReq) -> io.iePC,
    (csr.dcsr.reg.cause === Constants.DebugCause.GroupHartReq) -> io.iePC,
  ))
  */
  io.debug.dpc := csr.dpc.reg.data
  //csr.dpc.read(io.dpc)//csr.dpc.reg.data

  // WFI
  // FIXME: CLK Gating
  val wfi = RegInit(false.B)
  when (io.wfi.asUInt.orR && !wfi) {
    wfi := true.B
  }.elsewhen (mInterruptPending || exception) {
    wfi := false.B
  }
  io.wfiOut := wfi

  // CSR write results in stall and flush
  val writeFlush = !((req.ctrl.addr | (3.U << 8).asUInt) >= CSRAddr.mscratch.U &&
    (req.ctrl.addr | (3.U << 8).asUInt) <= CSRAddr.mtval.U)
  resp.csrWriteFlush := wen && writeFlush

  // To romiControl
  csr.mcause.read(io.csr.mcause)
  csr.mstatus.read(io.csr.mstatus)
  csr.mstatush.read(io.csr.mstatush)
  csr.mepc.read(io.csr.mepc)

  resp.rd := req.rd
}
