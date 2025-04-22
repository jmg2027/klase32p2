package klase32

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

import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.Growable._

// CSR control and R/W behavior

class CSRCtrl(implicit p: Parameters)
  extends CoreBundle
    with HasCoreParameters {
  val in = UInt(csrWidthM.W)
  val inst = CSRControl()
  val addr = UInt(12.W)
}

class CSRIntfIO(implicit p: Parameters)
  extends CoreBundle
    with HasCoreParameters {
  val k = p(KLASE32ParamKey)

  val ctrl = Input(new CSRCtrl())         // CSR control signal, op and address

  val wbData = Output(UInt(csrWidthM.W))  // writeback data from CSR to Register

  val exception = Input(Bool())           // ieXcpt from checkExceptions()
  val cause = Input(UInt(5.W))            // ieCause from checkExceptions()
  val exceptionME = Input(Bool())
  val causeME = Input(UInt(5.W))
  val iePC = Input(UInt(mxLen.W))          // epc from ie_pc
  val mePC = Input(UInt(mxLen.W))
  val evec = Output(UInt(mxLen.W))        // vector address of exception
  val ldAddrTrig = Input(UInt(k.vaddrBits.W))       // valid load address
  val stAddrTrig = Input(UInt(k.vaddrBits.W))       // valid store address
  val loadData = Input(UInt(k.dataWidth.W))       // load data
  val storeData = Input(UInt(k.dataWidth.W))       // store data

  val instr = Input(UInt(32.W))           // for tval, every cycle toggle

  val interrupt = Input(new Interrupt)    // interrupt from sw, timer, external

  val interruptPending = Output(Bool())   // interrupt pending from mstatus
  val interruptCause = Output(UInt(4.W))  // interrupt cause from mip and mie

  val hartId = Input(UInt(hartIDWidth.W)) // hartId

  val ecall = Input(EcallIE())            // ecall enable from decoder
  val ebreak = Input(EbreakIE())          // ebreak enable from decoder
  val mret = Input(MRetIE())              // mret enalbe from decoder
  val wfi = Input(WFIIE())                // wfi enable from decoder
  // for debugger
  val dret = Input(DRetIE())              // dret enable from decoder
//  val wdata = Input(UInt(mxLen.W))        // store data for trigger
  val dbgFire = Output(Bool())            // broadcasting debug fire
  val regDbgMode = Output(Bool())         // broadcasting debug mode
  val dpc = Output(UInt(mxLen.W))         // broadcasting dpc
  val regBooting = Input(Bool())          // for ResetHartRequest
  val bootAddr = Input(UInt(mxLen.W))     // for ResetHartRequest

  val trigEnterDmodeFire = Output(Bool())           // broadcast trigger dmode
  val trigFireLdData = Output(Bool())           // broadcast load trigger not to block writeback
  val trigRaiseBreakPointFireExe = Output(Bool())           // broadcast trigger bp
  val trigRaiseBreakPointFireLd = Output(Bool())           // broadcast trigger bp
  val trigRaiseBreakPointFireSt = Output(Bool())           // broadcast trigger bp

  val wfiOut = Output(Bool())             // for clock gating
  val csrWriteFlush = Output(Bool())      // transfer to Flush.IE

  val hpmIntf = new CSRPerfMonitorIntf
  val toRomiControl = new CSRToRomiControl

  // for debugger
  //val dpc = Input(UInt(mxLen.W))
}

class CSRPerfMonitorIntf (implicit p: Parameters) extends CoreBundle with HasCoreParameters {
  val ie_inst_valid = Input(Bool())
}

class CSRToRomiControl (implicit p: Parameters) extends CoreBundle with HasCoreParameters {
  val mcause = Output(UInt(mxLen.W))
  val mstatus = Output(UInt(mxLen.W))
  val mstatush = Output(UInt(mxLen.W))
  val mepc = Output(UInt(mxLen.W))
}


class CSRModule(implicit p: Parameters) extends CoreModule {
  import klase32.include.CSR._
  import CSRControl._

  val io = IO(new CSRIntfIO)
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
  csr.minstret.count(io.hpmIntf.ie_inst_valid && !exception, csr.mcounterinhibit.reg.asUInt(2))
  // When lower 32-bit counter overflows, high 32-bit register increases
  if (xLen == 32) {
    csr.mcycleh.count(csr.mcycle.reg.asUInt(xLen - 1), false.B)
    csr.minstreth.count(csr.minstret.reg.asUInt(xLen - 1), false.B)
  }

  val csrAddr = csrMap map {case (k, v) => k -> (io.ctrl.addr === k.U)}

  val a = for ((k, v) <- csrMap) yield (k)
  val b = for ((k, v) <- csrMap) yield (csrAddr(k) -> v)
  io.wbData := Mux1H(for ((k, v) <- csrMap) yield (csrAddr(k) -> v.asUInt))

  // FIXME: CSR illegal instruction check
  val wen = (io.ctrl.inst === RW) || (io.ctrl.inst === RS) || (io.ctrl.inst === RC)
  val wdata = Mux1H(Seq(
    (io.ctrl.inst === RW) -> io.ctrl.in,
    (io.ctrl.inst === RS) -> (io.ctrl.in | io.wbData),
    (io.ctrl.inst === RC) -> ((io.ctrl.in | io.wbData) & (~io.ctrl.in).asUInt),
    //    (io.ctrl.inst === RC) -> ((io.wbData) & (~io.ctrl.in).asUInt),
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
      trigFireExe := csr.tdata2.reg.data === io.instr    // Only for valid instruction
    }.elsewhen(trigExeAddrEn) {
//      trigFireExe := csr.tdata2.reg.data === io.iePC && (io.hpmIntf.ie_inst_valid)     // Only for valid instruction
      trigFireExe := csr.tdata2.reg.data === io.iePC     // Only for valid instruction
    }
    when(trigLdAddrEn) {
      trigFireLdAddr := csr.tdata2.reg.data === io.ldAddrTrig
    }
    when(trigLdDataEn) {
      trigFireLdData := csr.tdata2.reg.data === io.loadData
    }
    when(trigStAddrEn) {
      trigFireSt := csr.tdata2.reg.data === io.stAddrTrig
    }.elsewhen(trigStDataEn) {
      trigFireSt := csr.tdata2.reg.data === io.storeData
    }
  }

  trigFire := trigFireExe || trigFireSt || trigFireLdAddr || trigFireLdData
  io.trigRaiseBreakPointFireExe := (mcontrol6.action === Constants.ActionType.Breakpoint) && trigFireExe
  io.trigRaiseBreakPointFireLd  := (mcontrol6.action === Constants.ActionType.Breakpoint) && (trigFireLdAddr || trigFireLdData)
  io.trigRaiseBreakPointFireSt  := (mcontrol6.action === Constants.ActionType.Breakpoint) && trigFireSt
  io.trigEnterDmodeFire := (mcontrol6.action === Constants.ActionType.DMode) && trigFire
  io.trigFireLdData := trigFireLdData

  val ebreakToDebugMode = WireInit(false.B)
  ebreakToDebugMode := io.ebreak.asUInt.orR && ((csr.dcsr.reg.ebreaks && (priv===Constants.Privilege.SMode)) || (csr.dcsr.reg.ebreaku && (priv===Constants.Privilege.UMode)) || (csr.dcsr.reg.ebreakm && (priv===Constants.Privilege.MMode)))
  exception := io.ecall.asUInt.orR ||
    (io.ebreak.asUInt.orR && !ebreakToDebugMode) ||
    io.exception ||
    io.exceptionME

  val debugFire = WireInit(0.B)
  val singleStep = io.hpmIntf.ie_inst_valid && !regDebugMode && csr.dcsr.reg.step
  // After single step, PC must be fired by debugFire!
  debugFire := (io.interrupt.d && !regDebugMode) || singleStep || io.trigEnterDmodeFire
  regDebugMode := Mux(regDebugMode && io.dret.asUInt.orR, false.B, debugFire || regDebugMode)
  io.dbgFire := debugFire
  io.regDbgMode := regDebugMode

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
    io.cause === Causes.breakpoint.U ||
      io.cause === Causes.machine_ecall.U
  ) {
    tval := io.iePC
  }.elsewhen(io.cause === Causes.illegal_instruction.U) {
    tval := io.instr
  }.elsewhen(
    io.cause === Causes.misaligned_fetch.U ||
      io.cause === Causes.fetch_page_fault.U ||
      io.cause === Causes.fetch_guest_page_fault.U ||
      io.cause === Causes.fetch_access.U ||
      io.cause === Causes.misaligned_store.U ||
      io.cause === Causes.store_page_fault.U ||
      io.cause === Causes.store_guest_page_fault.U ||
      io.cause === Causes.store_access.U
  ) {
    // FIXME: Implement
    tval := 0.U
  }.elsewhen(
    io.cause === Causes.misaligned_load.U ||
      io.cause === Causes.load_page_fault.U ||
      io.cause === Causes.load_guest_page_fault.U ||
      io.cause === Causes.load_access.U
  ) {
    tval := io.ldAddrTrig
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
  io.interruptPending := mInterruptToBeTaken
  io.interruptCause := Mux1H (Seq(
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
    when(io.exceptionME) {
      csr.mepc.write(io.mePC)
      csr.mcause.write(io.causeME)
    }.otherwise {
      csr.mepc.write(io.iePC)
      csr.mcause.write(io.cause)
    }
    csr.mtval.write(tval)
    io.evec := Mux1H(Seq(
      (csr.mtvec.reg.mode === 0.U) -> (csr.mtvec.reg.base << 2.U),
      (csr.mtvec.reg.mode === 1.U) -> ((csr.mtvec.reg.base << 2.U) + (io.cause << 2.U))
    ))
  }.elsewhen (io.mret.asUInt.orR) {
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
  }.elsewhen(io.mret.asUInt.orR) {
    priv := csr.mstatus.reg.mpp
  }.elsewhen(io.dret.asUInt.orR) {
    priv := csr.dcsr.reg.prv
  }


  // dcsr update for debugger

  csr.dcsr.reg.prv := Mux(debugFire, priv, csr.dcsr.reg.prv)
  val dbgCause = WireInit(0.U(3.W))
  // Priority of dcsr.cause
  // resethaltreq -> haltgroup -> haltreq -> trigger -> ebreak -> step
  dbgCause := PriorityMux(Seq(
    (!singleStep && debugFire && io.regBooting) -> Constants.DebugCause.ResetHartReq,
    (!singleStep && debugFire && !io.regBooting) -> Constants.DebugCause.HartReq,
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
    (dbgCause === Constants.DebugCause.ResetHartReq) -> io.bootAddr,
    ((dbgCause === Constants.DebugCause.Trigger) && trigLdDataEn) -> (io.iePC + Mux(io.iePC(1), 2.U, 4.U)),
    (dbgCause === Constants.DebugCause.SingleStep) -> (io.iePC + Mux(io.iePC(1), 2.U, 4.U)),
    (dbgCause === Constants.DebugCause.EBreak) -> io.iePC,
    (dbgCause === Constants.DebugCause.HartReq) -> io.iePC,
    (dbgCause === Constants.DebugCause.GroupHartReq) -> io.iePC,
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
  io.dpc := csr.dpc.reg.data
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
  val writeFlush = !((io.ctrl.addr | 3.U << 8) >= CSRAddr.mscratch.U && (io.ctrl.addr | 3.U << 8) <= CSRAddr.mtval.U)
  io.csrWriteFlush := wen && writeFlush

  // To romiControl
  csr.mcause.read(io.toRomiControl.mcause)
  csr.mstatus.read(io.toRomiControl.mstatus)
  csr.mstatush.read(io.toRomiControl.mstatush)
  csr.mepc.read(io.toRomiControl.mepc)
}
