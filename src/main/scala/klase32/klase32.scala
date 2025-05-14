package klase32

import chisel3._
import chisel3.util._
import chisel3.util.BitPat.{bitPatToUInt, dontCare}
import klase32.include.config._
import klase32.include.enums.{OperandType, RdType}
import klase32.include.KLASE32AbstractClass._
import klase32.include.ControlSignal._
import klase32.include.Constants
import freechips.rocketchip.rocket.Causes
import klase32.functionalunit.CSRReq
import klase32.include.{EdmIntf, EpmIntf, Flush, Interrupt, Stall}

class KLASE32IO(implicit p: Parameters) extends CoreBundle
  with KLASE32IOEtc
  with KLASE32IORVFI
  with KLASE32IOToRomiControl {
  val interrupt = Input(new Interrupt)
  val edm = new EdmIntf
  val epm = new EpmIntf
}

trait KLASE32IOEtc { this: CoreBundle =>
  val hartEn = Input(Bool())
  val powerdown = Output(Bool())
  val forceStall = Input(Bool())
}

trait KLASE32IORVFI { this: CoreBundle =>
  val k = p(klase32.include.param.KLASE32ParamKey)
  val rvfiValid = Output(Bool())
  val rvfiPC = Output(UInt(mxLen.W))
  val rvfiIntreqEn = Output(Bool())
  val rvfiDebugEn = Output(Bool())
  val rvfiXcptEn = Output(Bool())

  val rvfiGPR = Vec(32, Output(UInt(mxLen.W)))

  val rvfiLdAck = Output(Bool())
  val rvfiLdAddr = Output(UInt(k.vaddrBits.W))
  val rvfiLdLen = Output(UInt(3.W))
  val rvfiLdRdata = Output(UInt(k.dataWidth.W))

  val rvfiStAck = Output(Bool())
  val rvfiStAddr = Output(UInt(k.vaddrBits.W))
  val rvfiStLen = Output(UInt(3.W))
  val rvfiStWdata = Output(UInt(k.dataWidth.W))
  val rvfiLdCmd = Output(UInt(5.W))
}

trait KLASE32IOToRomiControl {
  this: CoreBundle =>
  // Todo: Replace csr -> out signals to bundle
  val mcause = Output(UInt(mxLen.W))
  val mstatus = Output(UInt(mxLen.W))
  val mstatush = Output(UInt(mxLen.W))
  val mepc = Output(UInt(mxLen.W))
  val iepc = Output(UInt(mxLen.W))
}

class KLASE32(hartId: Int)(implicit p: Parameters) extends CoreModule
  with HasCoreParameters {

  val io = IO(new KLASE32IO)

  // Module definition
  val alu = Module(new klase32.functionalunit.ALU)
  val lsu = Module(new klase32.functionalunit.LSU)
  val scoreboard = Module(new Scoreboard)
  val div = Module(new klase32.functionalunit.DIV)
  val mpy = Module(new klase32.functionalunit.MPY)
  val frontend = Module(new Frontend)
  val bypass = Module(new Bypass)
  val csr = Module(new klase32.functionalunit.CSR)
  val hzd = Module(new Hazards)
  val dec = Module(new Decoder)
  val reg = Module(new RegisterFile())

  // control signal
  val ctrlSig = dec.io.decSig

  // Register writeback signal
//  val aluWriteBackValid = Wire(Bool())
//  val loadWriteBackValid = Wire(Bool())

  // Stall
  val functionUnitList = Seq(alu, mpy, div, lsu, scoreboard, csr)
  val stall  = fuList.map(!_.req.ready).reduce(_||_) || io.forceStall
//  val stallSig = WireInit(new Stall, DontCare)
//  stallSig.ie.storeFull := lsu.io.storeFull
//  stallSig.ie.mpy := false.B
//  stallSig.ie.div := false.B
//
//  if (writeportNum == 1) {
//    stallSig.ie.loadUseWriteBack := loadWriteBackValid
//    stallSig.me.loadFull := lsu.io.loadFull
//  } else if (writeportNum == 2) {
//    stallSig.ie.loadUseWriteBack := false.B
//    stallSig.me.loadFull := lsu.io.loadFull
//  }
//  stallSig.me.wfi := csr.io.wfiOut
//  stallSig.me.hzd := hzd.io.stall
//  stallSig.me.fence := ctrlSig.fence.asUInt.orR && (!lsu.io.loadEmpty || !lsu.io.storeEmpty)
//  val stallIE = stallSig.ie.orR
//  val stallME = stallSig.me.orR
//  val stall = stallIE || stallME || io.forceStall

  // Flush
  // FIXME: Can be optimized
  val flushSig = WireInit(new Flush, DontCare)
  flushSig.ie.jump := frontend.io.jump
  // When exception occurs, flush pipe and fetch queue since new instruction needed to be fetched from
  flushSig.ie.xcpt := ieXcpt || meXcpt
  flushSig.ie.eret := ctrlSig.ecall.asUInt.orR || ctrlSig.ebreak.asUInt.orR || ctrlSig.mret.asUInt.orR || ctrlSig.dret.asUInt.orR
  flushSig.ie.csr := csr.resp.bits.csrWriteFlush
  flushSig.ie.wfi := ctrlSig.wfi.asUInt.orR
  flushSig.ie.dbgFire := csr.io.debug.fire
  val flushIE = flushSig.ie.orR
  val flush = reset.asBool || flushIE

  // Pipeline
  val ieIn = frontend.io.issue
//  val ieSlotValid = RegInit(false.B)
//  val ieAllowNext = !stall
  ieIn.ready := !stall
//  when(flushSig.ie.asUInt.orR) {ieSlotValid := false.B}
//  when(ieAllowNext) {ieSlotValid := true.B}
  val ie_inst_raw = frontend.io.instRaw

  //  printf(cf"===============================New Cycle===============================\n")
  // printf(cf"ie_inst: ${ie_inst}%x\n")
  // printf(cf"ie_pc: ${ie_pc}%x\n")
  // printf(cf"ctrlSig: ${ctrlSig}\n")
  // printf(cf"stallSig.ie.issue: ${stallSig.ie.issue}\n")
  // printf(cf"stallSig.ie.store: ${stallSig.ie.store}\n")
  // printf(cf"stallSig.ie.csr: ${stallSig.ie.csr}\n")
  // printf(cf"stallSig.me.load: ${stallSig.me.load}\n")
  // printf(cf"stallSig.me.wfiOut: ${stallSig.me.wfiOut}\n")
  // printf(cf"stallSig.me.hzd: ${stallSig.me.hzd}\n")
  // printf(cf"stallSig.me.fence: ${stallSig.me.fence}\n")

  //  val me_inst = RegEnable(ie_inst, bitPatToUInt(NOP), !stallME)
//  val me_pc = withReset(flush) {RegEnable(ieIn.bits.pc, !stallME)}
//  val me_lsu = withReset(flush) {RegEnable(ctrlSig.lsuCtrl, !stallME)}
//  val me_isLoad = withReset(flush) {RegEnable(ctrlSig.lsuCtrl.isLoad, !stallME)}
//  val me_rdaddr = withReset(flush) {RegEnable(ctrlSig.rd, !stallME)}


  // Exception
  // Priority Mux is used for synchronous exceptions
  // Priority of interrupt is lower than exception (not on spec)
  // How to implement first encountered page fault or access fault?
  val (ieXcpt, ieCause): (Bool, UInt)  = checkExceptions(List(
    (csr.io.trigFire.breakPoint.exe, Causes.breakpoint.U),
    (frontend.io.issue.bits.xcpt.pf, Causes.fetch_page_fault.U),
    (frontend.io.issue.bits.xcpt.gf, Causes.fetch_guest_page_fault.U),
    (frontend.io.issue.bits.xcpt.ae, Causes.fetch_access.U),
    (ctrlSig.illegal === IllegalInstIE.illegal, Causes.illegal_instruction.U),
    (frontend.io.issue.bits.xcpt.ma, Causes.misaligned_fetch.U),
    (ctrlSig.ecall === EcallIE.EN, Causes.machine_ecall.U),
    (ctrlSig.ebreak === EbreakIE.EN, Causes.breakpoint.U),
    (csr.io.trigFire.breakPoint.store, Causes.breakpoint.U),

    // TODO: Need to discuss when and where to detect faults
    // This is temporal handling
    (lsu.resp.xcpt.pf, Causes.store_page_fault.U),
    (lsu.resp.xcpt.gf, Causes.store_guest_page_fault.U),
    (lsu.resp.xcpt.ae, Causes.store_access.U),
    (lsu.resp.xcpt.ma, Causes.misaligned_store.U),

    // TODO: Add conditions for instruction address breakpoints and data address breakpoints
    (csr.io.interruptPending.pending, csr.io.interruptPending.cause),
  ))

  val (meXcpt, meCause): (Bool, UInt)= checkExceptions(List(
    (csr.io.trigFire.loadData, Causes.breakpoint.U),
    (lsu.resp.fire && lsu.resp.xcpt.pf, Causes.load_page_fault.U),
    (lsu.resp.fire && lsu.resp.xcpt.gf, Causes.load_guest_page_fault.U),
    (lsu.resp.fire && lsu.resp.xcpt.ae, Causes.load_access.U),
    (lsu.resp.fire && lsu.resp.xcpt.ma, Causes.misaligned_load.U),
  ))

  // Fetch & Issue
  io.epm <> frontend.io.epm
  if (useInstKill) {
    io.epm.kill := flushSig
  } else {
    io.epm.kill := DontCare
  }

  // ctrl
  frontend.io.ctrl := ctrlSig.frontendCtrl
  frontend.io.flushIcache := ctrlSig.flushICache
  frontend.io.flushFetchQueue := flushSig ; dontTouch(frontend.io.flushFetchQueue)

  frontend.io.wfiOut := csr.io.wfiOut
  frontend.io.wfi := ctrlSig.wfi.asUInt.orR
  frontend.io.hartEn := io.hartEn

  frontend.io.evec := csr.io.evec
  frontend.io.cnd := alu.resp.bits.F
  frontend.io.exception := ieXcpt || meXcpt
  frontend.io.eret := ctrlSig.ecall.asUInt.orR || ctrlSig.ebreak.asUInt.orR || ctrlSig.mret.asUInt.orR

  frontend.io.aluR := Mux(frontend.io.ctrl === FrontendControlIE.JAL |
    frontend.io.ctrl === FrontendControlIE.JALR |
    frontend.io.ctrl === FrontendControlIE.MRET |
    frontend.io.ctrl === FrontendControlIE.DRET,
    alu.resp.bits.R, 0.U)

  frontend.io.brOffset := Mux(frontend.io.ctrl === FrontendControlIE.BR, ctrlSig.imm.b.asUInt, 0.U)
  frontend.io.ie_pc := ieIn.bits.pc

  // for debugger
  frontend.io.debug <> csr.io.debug
  frontend.io.ebreak := ctrlSig.ebreak.asUInt.orR
  frontend.io.dret := ctrlSig.dret.asUInt.orR

  // Decode
  dec.io.inst := ieIn.bits.data

  // Register file
  reg.io.rp(0).addr := ctrlSig.rs1
  reg.io.rp(1).addr := ctrlSig.rs2
  val bypassData = lsu.resp.bits.rddata
  val rs1 = Mux(!bypass.io.bypassRS1, reg.io.rp(0).data, bypassData)
  val rs2 = Mux(!bypass.io.bypassRS2, reg.io.rp(1).data, bypassData)
//
//  aluWriteBackValid := ((ctrlSig.w0Wb.asUInt.orR && !stall) || div.resp.valid) &&
//    (!(ieXcpt || meXcpt) && !csr.io.trigFire.debugMode)
//
//  loadWriteBackValid := me_isLoad.asUInt.orR && !stallME && !hzd.io.ldNotWriteback &&
//    ((!meXcpt && !csr.io.trigFire.debugMode) || ((meXcpt || csr.io.trigFire.debugMode) && csr.io.trigFire.loadData))

  class WritebackBundle(implicit p: Parameters) extends Bundle {
    val rd   = UInt(regIdWidth.W)
    val data = UInt(mxLen.W)
  }
  case class WbEntry(valid: Bool, bits: WritebackBundle)

  def WB(v: Bool, rd: UInt, d: UInt)(implicit p: Parameters): WbEntry = {
    val b = Wire(new WritebackBundle); b.rd := rd; b.data := d; WbEntry(v, b)
  }

  val wbBlock = ieXcpt || meXcpt || csr.io.debug.enterDmode

  val aluEntry = WB(alu.resp.valid , alu.resp.bits.rd , alu.resp.bits.R      )
  val mpyEntry = WB(mpy.resp.valid , mpy.resp.bits.rd , mpy.resp.bits.P      )
  val divEntry = WB(div.resp.valid , div.resp.bits.rd , div.resp.bits.result )
  val lsuEntry = WB(lsu.resp.fire  , lsu.resp.bits.rd , lsu.resp.bits.rddata )
  val csrEntry = WB(csr.resp.valid , csr.resp.bits.rd , csr.resp.bits.wbData )

  // 블로킹 적용
  val wbEntries = Seq(aluEntry, mpyEntry, divEntry, lsuEntry, csrEntry)
    .map(e => e.copy(valid = e.valid && !wbBlock))

  if (writeportNum == 1) {
    // LSU is first priority
    val wbList = Seq(lsuEntry) ++ wbEntries.filterNot(_ eq lsuEntry)
    val sel    = PriorityMux(wbList.map(e => e.valid -> e.bits))

    reg.io.wp(0).valid := wbList.map(_.valid).reduce(_||_)
    reg.io.wp(0).bits  := sel
  }
  else if(writeportNum == 2) {                     // 2-포트 : wp(1) = Load 전용
    // wp(1) — load only
    reg.io.wp(1).valid := lsuEntry.valid
    reg.io.wp(1).bits  := lsuEntry.bits

    // wp(0) — 나머지 FU 들
    val others = wbEntries.filterNot(_ eq lsuEntry)
    val sel    = PriorityMux(others.map(e => e.valid -> e.bits))

    reg.io.wp(0).valid := others.map(_.valid).reduce(_||_)
    reg.io.wp(0).bits  := sel
  }

  // MPY
  val mpyReq = Wire(new klase32.functionalunit.MPYReq)
  mpyReq.ctrl := ctrlSig.mpyCtrl
  mpyReq.A := rs1.asSInt
  mpyReq.B := rs2.asSInt
  mpyReq.rd := ctrlSig.rd

  mpy.req.bits := mpyReq
  mpy.req.valid := ieIn.valid && !stall && (ctrlSig.mpyCtrl =/= MPYControlIE.default)
  mpy.resp.ready := true.B

  // DIV
  val divReq = Wire(new klase32.functionalunit.DIVReq)
  divReq.ctrl := ctrlSig.divCtrl
  divReq.A := rs1.asSInt // Can use mux
  divReq.B := rs2.asSInt // Can use mux
  divReq.rd := ctrlSig.rd
//  div.io.A := Mux(div.io.ctrl =/= DIVControlIE.default, rs1.asSInt, 0.S) // Can use mux
//  div.io.B := Mux(div.io.ctrl =/= DIVControlIE.default, rs2.asSInt, 0.S) // Can use mux

  div.req.bits := divReq
  div.req.valid := ieIn.valid && !stall && (ctrlSig.divCtrl =/= DIVControlIE.default)
  div.resp.ready := true.B


  // ALU
  val aluReq = Wire(new klase32.functionalunit.ALUReq)
  aluReq.ctrl := ctrlSig.aluCtrl
  aluReq.A := Mux1H(Seq(
    OperandType.None -> 0.U,
    OperandType.Reg -> rs1,
    OperandType.PC -> ieIn.bits.pc,
    OperandType.CSRImmediate -> ctrlSig.rs1 // rs1 field of instruction is imm field
  ).map { case (k, v) => (k === ctrlSig.operandSelect.a, v) }
  )

  aluReq.B := Mux1H(Seq(
    OperandType.None -> 0.U,
    OperandType.Reg -> rs2,
    OperandType.IImmediate -> ctrlSig.imm.i.asUInt,
    OperandType.UImmediate -> ctrlSig.imm.u.asUInt,
    OperandType.JImmediate -> ctrlSig.imm.j.asUInt,
    OperandType.SImmediate -> ctrlSig.imm.s.asUInt
  ).map { case (k, v) => (k === ctrlSig.operandSelect.b, v) }
  )

  aluReq.rd := ctrlSig.rd

  alu.req.bits := aluReq
  alu.req.valid := ieIn.valid && !stall && ctrlSig.aluCtrl =/= ALUControlIE.default
  alu.resp.ready := true.B

  // CSR
  val csrReq = Wire(new CSRReq())
  csrReq.ctrl.inst := ctrlSig.csrCtrl
  csrReq.ctrl.in := Mux(csr.req.valid, alu.resp.bits.R, 0.U)
  csrReq.ctrl.addr := Mux(csr.req.valid, ieIn.bits.data(31, 20), 0.U)
  csrReq.rd := Mux(csr.req.valid, ctrlSig.rd, 0.U)

  csr.req.bits := csrReq
  csr.req.valid := (csr.req.bits.ctrl.inst === CSRControl.RW) || (csr.req.bits.ctrl.inst === CSRControl.RS) || (csr.req.bits.ctrl.inst === CSRControl.RC)

  csr.resp.ready := true.B

  csr.io.hartId := hartId.U
  csr.io.sys.ecall := ctrlSig.ecall
  csr.io.sys.ebreak := ctrlSig.ebreak
  csr.io.ret.mret := ctrlSig.mret
  csr.io.wfi := ctrlSig.wfi
  // for debugger
  csr.io.ret.dret := ctrlSig.dret
  csr.io.resetReq <> frontend.io.resetReq

  // Used to tval register update when illegal instruction exception occurs
  // And trigger, should this be use raw instructions including RVC
  csr.io.trigSrc.instruction := ie_inst_raw
  csr.io.trigSrc.pc := ieIn.bits.pc

  csr.io.ie_inst_valid := ieIn.valid

  // FIXME: When speculative load/store supported, exception should be failed load/store pc
  csr.io.trap.ie.exception := ieXcpt
  csr.io.trap.ie.cause := ieCause
  csr.io.trap.me.exception := meXcpt
  csr.io.trap.me.cause := meCause
  csr.io.trap.ie.pc := ieIn.bits.pc
  csr.io.trap.me.pc := me_pc
  csr.io.trigSrc.loadAddr := Mux(lsu.req.ctrl.isLoad === LoadControl.EN, lsu.req.addr, 0.U)
  csr.io.trigSrc.storeAddr := Mux(lsu.req.ctrl.isStore === StoreControl.EN, lsu.req.addr, 0.U)
  csr.io.trigSrc.loadData := lsu.resp.rddata // valid check in LSU when ld_ack is recieved
  csr.io.trigSrc.storeData := Mux(lsu.req.ctrlisStore === StoreControl.EN, lsu.req.wrdata, 0.U)

  io.powerdown := csr.io.wfiOut

  // interrupt
  csr.io.interrupt := io.interrupt

  io.mcause := csr.io.csr.mcause
  io.mstatus := csr.io.csr.mstatus
  if (xLen == 32) {
    io.mstatush := csr.io.csr.mstatush
  }
  io.mepc := csr.io.csr.mepc
  io.iepc := ieIn.bits.pc

  // Scoreboard
  scoreboard.io.rs1 := ctrlSig.rs1
  scoreboard.io.rs2 := ctrlSig.rs2
  scoreboard.io.set.valid   := lsu.req.fire && ctrlSig.lsuCtrl.isLoad
  scoreboard.io.set.bits    := ctrlSig.rd
  scoreboard.io.clear.valid := lsu.resp.fire
  scoreboard.io.clear.bits  := lsu.resp.bits.rd

  // Bypass
  bypass.io.wb.valid := lsu.resp.fire
  bypass.io.wb.bits := lsu.resp.bits.rd
  bypass.io.rs1 := ctrlSig.rs1
  bypass.io.rs2 := ctrlSig.rs2

  // LSU
  lsu.req.valid      := ieIn.valid && !stall &&
                        (ctrlSig.lsuCtrl.isLoad.orR || ctrlSig.lsuCtrl.isStore.orR)
  lsu.req.bits.ctrl  := ctrlSig.lsuCtrl
  lsu.req.bits.addr  := alu.resp.bits.R
  lsu.req.bits.wrdata:= rs2
  lsu.req.bits.fence := ctrlSig.fence
  lsu.req.bits.rd    := ctrlSig.rd

  lsu.resp.ready := true.B           // WB-mux가 항상 수신

  lsu.io.edm <> io.edm

  // RVFI
  io.rvfiValid := ieIn.valid
  io.rvfiPC := ieIn.bits.pc
  io.rvfiIntreqEn := DontCare
  io.rvfiDebugEn := DontCare
  io.rvfiXcptEn := DontCare

  io.rvfiGPR := DontCare

  io.rvfiLdAck := DontCare
  io.rvfiLdAddr := DontCare
  io.rvfiLdLen := DontCare
  io.rvfiLdRdata := DontCare

  io.rvfiStAck := DontCare
  io.rvfiStAddr := DontCare
  io.rvfiStLen := DontCare
  io.rvfiStWdata := DontCare
  io.rvfiLdCmd := DontCare

  def checkExceptions(x: Seq[(Bool, UInt)]) =
    (x.map(_._1).reduce(_||_), PriorityMux(x))
}
