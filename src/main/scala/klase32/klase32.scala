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
import klase32.include.{EdmIntf, EpmIntf, Flush, Interrupt, Stall}

class KLASE32IO(implicit p: Parameters) extends CoreBundle
  with KLASE32IOEtc
  with KLASE32IORVFI
  with KLASE32IOToRomiControl {
  // val acc = new Acc.Interface
  // val interrupt = Input(new HeartIntReq)
  val interrupt = Input(new Interrupt)
  val edm = new EdmIntf
  val epm = new EpmIntf
  //  val jtag = new JtagIntf
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
  val alu = Module(new ALU)
  val lsu = Module(new LSU)
  val div = Module(new DIV)
  val mpy = Module(new MPY)
  val frontend = Module(new Frontend)
  val csr = Module(new CSRModule)
  val hzd = Module(new Hazards)
  val dec = Module(new Decoder)
  val reg = Module(new RegisterFile())

  // control signal
  val ctrlSig = dec.io.decSig

  // Register writeback signal
  val aluWriteBackValid = Wire(Bool())
  val loadWriteBackValid = Wire(Bool())

  // Stall
  val stallSig = WireInit(new Stall, DontCare)
  stallSig.ie.storeFull := lsu.io.storeFull
  stallSig.ie.mpy := mpy.io.busy
  stallSig.ie.div := div.io.busy
//  if (writeportNum == 1) { //    stallSig.ie.load := loadWriteBackValid
//  } else if (writeportNum == 2) {
//    stallSig.ie.load := false.B
//  }

  //  for timing issue stall 1 cycle for exceptions (Temp)

  if (writeportNum == 1) {
    stallSig.ie.loadUseWriteBack := loadWriteBackValid
    stallSig.me.loadFull := lsu.io.loadFull
  } else if (writeportNum == 2) {
    stallSig.ie.loadUseWriteBack := false.B
    stallSig.me.loadFull := lsu.io.loadFull
  }
  stallSig.me.wfi := csr.io.wfiOut
  stallSig.me.hzd := hzd.io.stall
  stallSig.me.fence := ctrlSig.fence.asUInt.orR && (!lsu.io.loadEmpty || !lsu.io.storeEmpty)
  val stallIE = stallSig.ie.orR
  val stallME = stallSig.me.orR
  val stall = stallIE || stallME || io.forceStall

  // Flush
  // FIXME: Can be optimized
  val flushSig = WireInit(new Flush, DontCare)
  flushSig.ie.jump := frontend.io.jump
  // When exception occurs, flush pipe and fetch queue since new instruction needed to be fetched from
  flushSig.ie.xcpt := frontend.io.exception
  flushSig.ie.eret := ctrlSig.ecall.asUInt.orR || ctrlSig.ebreak.asUInt.orR || ctrlSig.mret.asUInt.orR || ctrlSig.dret.asUInt.orR
  flushSig.ie.csr := csr.io.csrWriteFlush
  flushSig.ie.wfi := ctrlSig.wfi.asUInt.orR
//  flushSig.ie.fence := ctrlSig.fence.asUInt.orR && (lsu.io.loadFull || lsu.io.storeEmpty)
  flushSig.ie.dbgFire := csr.io.dbgFire.orR
  val flushIE = flushSig.ie.orR
  val flush = reset.asBool || flushIE

  // Pipeline
  val ie_pc =  frontend.io.fetchedPacket.bits.pc
  val ie_inst_valid = frontend.io.fetchedPacket.valid
  val ie_inst = frontend.io.fetchedPacket.bits.data
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
  val me_pc = withReset(flush) {RegEnable(ie_pc, !stallME)}
  val me_lsu = withReset(flush) {RegEnable(ctrlSig.lsuCtrl, !stallME)}
  val me_isLoad = withReset(flush) {RegEnable(ctrlSig.lsuCtrl.isLoad, !stallME)}
  val me_rdaddr = withReset(flush) {RegEnable(ctrlSig.rd, !stallME)}
  //  val me_aluR = withReset(flush) {RegEnable(alu.io.R, !stallME)}


  // Exception
  // Priority Mux is used for synchronous exceptions
  // Priority of interrupt is lower than exception (not on spec)
  // How to implement first encountered page fault or access fault?
  val (ieXcpt, ieCause): (Bool, UInt)  = checkExceptions(List(
    (csr.io.trigRaiseBreakPointFireExe, Causes.breakpoint.U),
    (frontend.io.fetchedPacket.bits.xcpt.pf, Causes.fetch_page_fault.U),
    (frontend.io.fetchedPacket.bits.xcpt.gf, Causes.fetch_guest_page_fault.U),
    (frontend.io.fetchedPacket.bits.xcpt.ae, Causes.fetch_access.U),
    (ctrlSig.illegal === IllegalInstIE.illegal, Causes.illegal_instruction.U),
    (frontend.io.fetchedPacket.bits.xcpt.ma, Causes.misaligned_fetch.U),
    (ctrlSig.ecall === EcallIE.EN, Causes.machine_ecall.U),
    (ctrlSig.ebreak === EbreakIE.EN, Causes.breakpoint.U),
    (csr.io.trigRaiseBreakPointFireSt, Causes.breakpoint.U),

    (lsu.io.stXcpt.pf, Causes.store_page_fault.U),
    (lsu.io.stXcpt.gf, Causes.store_guest_page_fault.U),
    (lsu.io.stXcpt.ae, Causes.store_access.U),
    (lsu.io.stXcpt.ma, Causes.misaligned_store.U),

    // TODO: Add conditions for instruction address breakpoints and data address breakpoints
    (csr.io.interruptPending, csr.io.interruptCause),
  ))

  val (meXcpt, meCause): (Bool, UInt)= checkExceptions(List(
    (csr.io.trigRaiseBreakPointFireLd, Causes.breakpoint.U),
    (lsu.io.ldXcpt.pf, Causes.load_page_fault.U),
    (lsu.io.ldXcpt.gf, Causes.load_guest_page_fault.U),
    (lsu.io.ldXcpt.ae, Causes.load_access.U),
    (lsu.io.ldXcpt.ma, Causes.misaligned_load.U),
  ))

  // Delay 1 cycle for exception due to timing
//  val regIeXcpt = RegNext(ieXcpt)
//  val regMeXcpt = RegNext(meXcpt)
//  val regEvec = RegNext(csr.io.evec)
//  val regEret = RegNext(ctrlSig.ecall.asUInt.orR || ctrlSig.ebreak.asUInt.orR || ctrlSig.mret.asUInt.orR)
//  val regIeCause = RegNext(ieCause)
//  val regMeCause = RegNext(meCause)


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
//  frontend.io.evec := regEvec
  frontend.io.cnd := alu.io.F
  frontend.io.exception := ieXcpt || meXcpt
//  frontend.io.exception := regIeXcpt || regMeXcpt
  frontend.io.eret := ctrlSig.ecall.asUInt.orR || ctrlSig.ebreak.asUInt.orR || ctrlSig.mret.asUInt.orR
//  frontend.io.eret := regEret

  frontend.io.stall := stall

  frontend.io.aluR := Mux(frontend.io.ctrl === FrontendControlIE.JAL |
    frontend.io.ctrl === FrontendControlIE.JALR |
    frontend.io.ctrl === FrontendControlIE.MRET |
    frontend.io.ctrl === FrontendControlIE.DRET,
    alu.io.R, 0.U)

  frontend.io.brOffset := Mux(frontend.io.ctrl === FrontendControlIE.BR, ctrlSig.imm.b.asUInt, 0.U)
  frontend.io.ie_pc := ie_pc

  // for debugger
  frontend.io.dbgFire := csr.io.dbgFire
  frontend.io.regDbgMode := csr.io.regDbgMode
  frontend.io.ebreak := ctrlSig.ebreak.asUInt.orR
  frontend.io.dret := ctrlSig.dret.asUInt.orR
  frontend.io.dpc := csr.io.dpc

  // Decode
  dec.io.inst := ie_inst

  // Register file
  reg.io.rp(0).addr := ctrlSig.rs1
  reg.io.rp(1).addr := ctrlSig.rs2
  val rs1 = Mux(!hzd.io.bypassRS1RD, reg.io.rp(0).data, lsu.io.rddata)
  val rs2 = Mux(!hzd.io.bypassRS2RD, reg.io.rp(1).data, lsu.io.rddata)

//  aluWriteBackValid := ((ctrlSig.w0Wb.asUInt.orR && ie_inst_valid) || div.io.res.valid) &&
  aluWriteBackValid := ((ctrlSig.w0Wb.asUInt.orR && !stall) || div.io.res.valid) &&
    (!(ieXcpt || meXcpt) && !csr.io.trigEnterDmodeFire)

  loadWriteBackValid := me_isLoad.asUInt.orR && !stallME && !hzd.io.ldNotWriteback &&
    ((!meXcpt && !csr.io.trigEnterDmodeFire) || ((meXcpt || csr.io.trigEnterDmodeFire) && csr.io.trigFireLdData))

  if (writeportNum == 2) {
    reg.io.wp(0).bits.addr := ctrlSig.rd
    reg.io.wp(1).bits.addr := me_rdaddr
    // When exception occurs, do not writeback current
    // When load data/address triggers, write back load data
    reg.io.wp(0).valid := aluWriteBackValid
    // When load wbData is same as wbData of ie pipe instruction, most recent value which updates register is not load data
    reg.io.wp(1).valid := loadWriteBackValid
    //  (!meXcpt && !csr.io.trigEnterDmodeFire)
    reg.io.wp(0).bits.data := Mux1H(Seq(
      RdType.Alu -> alu.io.R,
      RdType.ConsecPC -> (frontend.io.ie_pc + Mux(frontend.io.fetchedPacket.bits.rvc, 2.U, 4.U)),
      RdType.BypassCSR -> csr.io.wbData
    ).map {case(k, v) => (k === ctrlSig.rdType, v)} :+
      (div.io.res.valid -> div.io.res.bits) :+
      (mpy.io.prod.valid -> mpy.io.prod.bits)
    )
    reg.io.wp(1).bits.data := lsu.io.rddata
  } else if(writeportNum == 1) {
    reg.io.wp(0).bits.addr := Mux(loadWriteBackValid, me_rdaddr, ctrlSig.rd)
    reg.io.wp(0).valid := (aluWriteBackValid && !loadWriteBackValid) || (loadWriteBackValid && lsu.io.canLoadWriteback)

    val aluWriteBackData = Mux1H(Seq(
      RdType.Alu -> alu.io.R,
      RdType.ConsecPC -> (frontend.io.ie_pc + Mux(frontend.io.fetchedPacket.bits.rvc, 2.U, 4.U)),
      RdType.BypassCSR -> csr.io.wbData
    ).map { case (k, v) => (k === ctrlSig.rdType, v) } :+
      (div.io.res.valid -> div.io.res.bits) :+
      (mpy.io.prod.valid -> mpy.io.prod.bits)
    )
    val loadWriteBackData = lsu.io.rddata
    reg.io.wp(0).bits.data := Mux(loadWriteBackValid, loadWriteBackData, aluWriteBackData)

//    when (loadWriteBackValid) {
//      reg.io.wp(0).bits.data := loadWriteBackData
//    }.elsewhen(aluWriteBackValid) {
//      reg.io.wp(0).bits.data := aluWriteBackData
//    }
  }


  // MPY
  mpy.io.ctrl := ctrlSig.mpyCtrl
  //  mpy.io.A := rs1.asSInt // Can use mux
  //  mpy.io.B := rs2.asSInt // Can use mux
  mpy.io.A := Mux(mpy.io.ctrl =/= MPYControlIE.default, rs1.asSInt, 0.S)
  mpy.io.B := Mux(mpy.io.ctrl =/= MPYControlIE.default, rs2.asSInt, 0.S)
  mpy.io.fusedMul := false.B // FIXME: Fused multiply

  // DIV
  div.io.ctrl := ctrlSig.divCtrl
  //  div.io.A := rs1.asSInt // Can use mux
  //  div.io.B := rs2.asSInt // Can use mux
  div.io.A := Mux(div.io.ctrl =/= DIVControlIE.default, rs1.asSInt, 0.S) // Can use mux
  div.io.B := Mux(div.io.ctrl =/= DIVControlIE.default, rs2.asSInt, 0.S) // Can use mux

  // ALU
  alu.io.ctrl := ctrlSig.aluCtrl

  alu.io.A := Mux1H(Seq(
    OperandType.None -> 0.U,
    OperandType.Reg -> rs1,
    OperandType.PC -> ie_pc,
    OperandType.CSRImmediate -> ctrlSig.rs1 // rs1 field of instruction is imm field
  ).map { case (k, v) => (k === ctrlSig.operandSelect.a, v) }
  )

  alu.io.B := Mux1H(Seq(
    OperandType.None -> 0.U,
    OperandType.Reg -> rs2,
    OperandType.IImmediate -> ctrlSig.imm.i.asUInt,
    OperandType.UImmediate -> ctrlSig.imm.u.asUInt,
    OperandType.JImmediate -> ctrlSig.imm.j.asUInt,
    OperandType.SImmediate -> ctrlSig.imm.s.asUInt
  ).map { case (k, v) => (k === ctrlSig.operandSelect.b, v) }
  )

  // printf(cf"ctrlSig.operandSelect.a: ${ctrlSig.operandSelect.a}\n")
  // printf(cf"ctrlSig.operandSelect.b: ${ctrlSig.operandSelect.b}\n")
  // printf(cf"alu.io.ctrl: ${alu.io.ctrl}\n")
  // printf(cf"alu.io.A: ${alu.io.A}%x\n")
  // printf(cf"alu.io.B: ${alu.io.B}%x\n")


  // CSR
  csr.io.ctrl.inst := ctrlSig.csrCtrl
  val csrWen = (csr.io.ctrl.inst === CSRControl.RW) || (csr.io.ctrl.inst === CSRControl.RS) || (csr.io.ctrl.inst === CSRControl.RC)
  //  csr.io.ctrl.in := alu.io.R
  //  csr.io.ctrl.addr := ie_inst(31, 20)
  csr.io.ctrl.in := Mux(csrWen, alu.io.R, 0.U)
  csr.io.ctrl.addr := Mux(csrWen, ie_inst(31, 20), 0.U)

  csr.io.hartId := hartId.U
  csr.io.ecall := ctrlSig.ecall
  csr.io.ebreak := ctrlSig.ebreak
  csr.io.mret := ctrlSig.mret
  csr.io.wfi := ctrlSig.wfi
  // for debugger
  csr.io.dret := ctrlSig.dret
  //  csr.io.wdata := rs2
  csr.io.regBooting := frontend.io.regBooting
  csr.io.bootAddr := frontend.io.bootAddr

  //  when(!flush) {
  //    ie_inst_raw := frontend.io.if_inst_raw
  //  }.otherwise {
  //    ie_inst_raw := ie_inst_raw
  //  }
  // Used to tval register update when illegal instruction exception occurs
  // And trigger, should this be use raw instructions including RVC
  csr.io.instr := ie_inst_raw

  csr.io.hpmIntf.ie_inst_valid := ie_inst_valid

  // FIXME: When speculative load/store supported, exception should be failed load/store pc
  csr.io.exception := ieXcpt
  csr.io.cause := ieCause
  csr.io.exceptionME := meXcpt
  csr.io.causeME := meCause
  csr.io.iePC := ie_pc
  csr.io.mePC := me_pc
  csr.io.ldAddrTrig := Mux(lsu.io.lsuctrlIE.isLoad === LoadControl.EN, lsu.io.addr, 0.U)
  csr.io.stAddrTrig := Mux(lsu.io.lsuctrlIE.isStore === StoreControl.EN, lsu.io.addr, 0.U)
  csr.io.loadData := lsu.io.rddata // valid check in LSU when ld_ack is recieved
  csr.io.storeData := Mux(lsu.io.lsuctrlIE.isStore === StoreControl.EN, lsu.io.wrdata, 0.U)

  io.powerdown := csr.io.wfiOut

  // interrupt
  csr.io.interrupt := io.interrupt

  io.mcause := csr.io.toRomiControl.mcause
  io.mstatus := csr.io.toRomiControl.mstatus
  if (xLen == 32) {
    io.mstatush := csr.io.toRomiControl.mstatush
  }
  io.mepc := csr.io.toRomiControl.mepc
  io.iepc := ie_pc

  // Hazard
  // FIXME: Why does this not work?
  hzd.io.rs1Valid := ((OperandType.Reg) === (ctrlSig.operandSelect.a))
  // RS2: ALU.B || Store || Branch
  hzd.io.rs2Valid := ((ctrlSig.operandSelect.b) === (OperandType.Reg)) ||
    ((ctrlSig.frontendCtrl) === (FrontendControlIE.BR)) ||
    ((ctrlSig.lsuCtrl.isStore) === (StoreControl.EN))
  hzd.io.loadValidIE := lsu.io.lsuctrlIE.isLoad === LoadControl.EN
  hzd.io.loadValidME := me_isLoad === LoadControl.EN
  hzd.io.loadDataValidME := lsu.io.canLoadWriteback

  hzd.io.divBusy := div.io.busy
  hzd.io.divAddr := ctrlSig.rd
  hzd.io.divWrite := div.io.res.valid

  hzd.io.rs1Addr := ctrlSig.rs1
  hzd.io.rs2Addr := ctrlSig.rs2
  hzd.io.rdAddrME := me_rdaddr
  // When rd is not used and does not write back, it must avoid to determine hazard condition
  hzd.io.rdAddrIE := Mux(ctrlSig.w0Wb.asUInt.orR || ctrlSig.lsuCtrl.isLoad.asUInt.orR, ctrlSig.rd, 0.U)
  // lsu.io.ldKill := hzd.io.ldNotWriteback

  hzd.io.loadFull := lsu.io.loadFull
  hzd.io.loadAck := io.edm.ld_ack

  // LSU
  lsu.io.lsuctrlIE <> ctrlSig.lsuCtrl
  lsu.io.lsuctrlME <> me_lsu

  lsu.io.edm <> io.edm
  //  lsu.io.addr := alu.io.R
  //  lsu.io.wrdata := rs2
  lsu.io.addr := Mux(lsu.io.lsuctrlIE.isLoad === LoadControl.EN | lsu.io.lsuctrlIE.isStore === StoreControl.EN,
    alu.io.R, 0.U)
  lsu.io.wrdata := Mux(lsu.io.lsuctrlIE.isStore === StoreControl.EN, rs2, 0.U)

  // printf(cf"lsu.io.addr: ${lsu.io.addr}%x\n")
  // printf(cf"lsu.io.wrdata: ${lsu.io.wrdata}%x\n")
  lsu.io.stall := stall
  lsu.io.stallME := stallME

  // RVFI
  io.rvfiValid := ie_inst_valid
  io.rvfiPC := ie_pc
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
