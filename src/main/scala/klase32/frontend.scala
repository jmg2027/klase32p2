package klase32

import chisel3._
import chisel3.util._
import chisel3.util.BitPat.bitPatToUInt
import klase32.config._
import snitch.enums.{OperandType, RdType}
import freechips.rocketchip.rocket.Causes

class KLASE32IO(implicit p: Parameters) extends Bundle with KLASE32IOEtc {
  //  val acc = new Acc.Interface
  val interrupt = Input(new Interrupt)
  val edm = new EdmIntf
  val epm = new EpmIntf
  //  val jtag = new JtagIntf
  //  val dbg = new DbgIntf
}

trait KLASE32IOEtc extends Bundle {
  val powerdown = Output(Bool())
}

class KLASE32(hartId: Int)(implicit p: Parameters) extends CoreModule
  with HasCoreParameters {
  val NOP = BitPat("b00000000000000000000000000010011")

  val io = IO(new KLASE32IO)


  // Module definition
  val alu = Module(new ALU)
  val lsu = Module(new LSU)
  //val div = Module(new DIV)
  //val mpy = Module(new MPY)
  val frontend = Module(new Frontend)
  val csr = Module(new CSRModule)
  val hzd = Module(new Hazards)
  //val dbgCtrl = Module(new DebugController)
  //val jtag = Module(new JtagInterface)
  val dec = Module(new Decoder)
  val reg = Module(new RegisterFile())


  // control signal
  val ctrlSig = dec.io.decSig

  // Stall
  val stallSig = WireInit(new Stall, DontCare)
  stallSig.ie.issue := !frontend.io.issue
  stallSig.ie.store := lsu.io.storeFull
  // stallSig.ie.csr := csr.io.csrWrite
  stallSig.me.load := lsu.io.loadFull
  stallSig.me.wfi := csr.io.wfiOut
  stallSig.me.hzd := hzd.io.stall
  stallSig.me.fence := ctrlSig.fence.asUInt.orR && (lsu.io.loadFull || lsu.io.storeFull)
  val stallIE = stallSig.ie.orR
  val stallME = stallSig.me.orR
  val stall = stallIE || stallME

  // Flush
  // FIXME: Can be optimized
  val flushSig = WireInit(new Flush, DontCare)
  flushSig.ie.jump := frontend.io.jump
  // flushSig.ie.xcpt := meXcpt
  flushSig.ie.xcpt := frontend.io.exception
  flushSig.ie.eret := ctrlSig.ecall.asUInt.orR || ctrlSig.ebreak.asUInt.orR || ctrlSig.mret.asUInt.orR
  flushSig.ie.csr := csr.io.csrWrite
  flushSig.ie.fence := ctrlSig.fence.asUInt.orR && (lsu.io.loadFull || lsu.io.storeFull)
  val flushIE = flushSig.ie.orR
  val flush = reset.asBool || flushIE

  // Pipeline
  //  val ie_inst = RegEnable(frontend.io.instPacket.inst, bitPatToUInt(NOP), !stall && frontend.io.issue)
  val ie_inst = withReset(flush) {RegEnable(frontend.io.instPacket.bits.data, bitPatToUInt(NOP), !stall)}
  val ie_pc = withReset(flush) {RegEnable(frontend.io.if_pc, !stall)}

  printf(cf"===============================New Cycle===============================\n")
  // printf(cf"ie_inst: ${ie_inst}%x\n")
  // printf(cf"ie_pc: ${ie_pc}%x\n")
  // printf(cf"ctrlSig: ${ctrlSig}\n")
  // printf(cf"stallSig.ie.issue: ${stallSig.ie.issue}\n")
  // printf(cf"stallSig.ie.store: ${stallSig.ie.store}\n")
  // printf(cf"stallSig.ie.csr: ${stallSig.ie.csr}\n")
  // printf(cf"stallSig.me.load: ${stallSig.me.load}\n")
  // printf(cf"stallSig.me.wfi: ${stallSig.me.wfi}\n")
  // printf(cf"stallSig.me.hzd: ${stallSig.me.hzd}\n")
  // printf(cf"stallSig.me.fence: ${stallSig.me.fence}\n")

  //  val me_inst = RegEnable(ie_inst, bitPatToUInt(NOP), !stallME)
  val me_pc = withReset(flush) {RegEnable(ie_pc, !stallME)}
  val me_lsu = withReset(flush) {RegEnable(ctrlSig.lsuCtrl, !stallME)}
  val me_isLoad = withReset(flush) {RegEnable(ctrlSig.lsuCtrl.isLoad, !stallME)}
  val me_rdaddr = withReset(flush) {RegEnable(ctrlSig.rd, !stallME)}
  val me_aluR = withReset(flush) {RegEnable(alu.io.R, !stallME)}


  // Exception
  val (ieXcpt, ieCause): (Bool, UInt)  = checkExceptions(List(
    (csr.io.interruptPending, csr.io.interruptCause),
    (frontend.io.instPacket.bits.xcpt.ma, Causes.misaligned_fetch.U),
    (frontend.io.instPacket.bits.xcpt.pf, Causes.fetch_page_fault.U),
    (frontend.io.instPacket.bits.xcpt.gf, Causes.fetch_guest_page_fault.U),
    (frontend.io.instPacket.bits.xcpt.ae, Causes.fetch_access.U),

    (lsu.io.stXcpt.ma, Causes.misaligned_store.U),
    (lsu.io.stXcpt.pf, Causes.store_page_fault.U),
    (lsu.io.stXcpt.gf, Causes.store_guest_page_fault.U),
    (lsu.io.stXcpt.ae, Causes.store_access.U),
    (ctrlSig.illegal === IllegalInstIE.illegal, Causes.illegal_instruction.U),
    (ctrlSig.ecall === EcallIE.EN, Causes.machine_ecall.U),
    (ctrlSig.ebreak === EbreakIE.EN, Causes.breakpoint.U),

    // CHECK: No need to pipeline for load exception...
    (lsu.io.ldXcpt.ma, Causes.misaligned_load.U),
    (lsu.io.ldXcpt.pf, Causes.load_page_fault.U),
    (lsu.io.ldXcpt.gf, Causes.load_guest_page_fault.U),
    (lsu.io.ldXcpt.ae, Causes.load_access.U),
  ))
//
//  // Exception pipeline
//  val ieXcptReg = RegEnable(ieXcpt, !stallME)
//  val ieCauseReg = RegEnable(ieCause, !stallME)
//
//  // FIXME: Until cache
//  val (meXcpt, meCause): (Bool, UInt)= checkExceptions(List(
//    (ieXcptReg, ieCauseReg),
//    (lsu.io.ldXcpt.ma, Causes.misaligned_load.U),
//    (lsu.io.ldXcpt.pf, Causes.load_page_fault.U),
//    (lsu.io.ldXcpt.gf, Causes.load_guest_page_fault.U),
//    (lsu.io.ldXcpt.ae, Causes.load_access.U),
//  ))


  // Fetch & Issue
  io.epm <> frontend.io.epm
  io.epm.kill := DontCare


  // ctrl
  frontend.io.ctrl := ctrlSig.frontendCtrl
  frontend.io.flushIcache := ctrlSig.flushICache
  frontend.io.flushFetchQueue := flushSig ; dontTouch(frontend.io.flushFetchQueue)

  frontend.io.divBusy := DontCare
  frontend.io.wfi := csr.io.wfiOut

  frontend.io.evec := csr.io.evec
  frontend.io.cnd := alu.io.F
  // FIXME: Should handle this
  // frontend.io.exception := meXcpt
  frontend.io.exception := ieXcpt
  frontend.io.eret := ctrlSig.ecall.asUInt.orR || ctrlSig.ebreak.asUInt.orR || ctrlSig.mret.asUInt.orR

  frontend.io.stall := stall

  frontend.io.aluR := alu.io.R
  frontend.io.brOffset := ctrlSig.imm.b.asUInt
  frontend.io.ie_pc := ie_pc

  // Decode
  dec.io.inst := ie_inst


  // Register file
  reg.io.rp(0).addr := ctrlSig.rs1
  reg.io.rp(1).addr := ctrlSig.rs2
  // FIXME: Why this prints 0??
  val rs1 = Mux(!hzd.io.bypassRS1RD, reg.io.rp(0).data, lsu.io.rddata)
  //  val rs1 = reg.io.rp(0).data
  // printf(cf"reg.io.rp(0).addr: ${reg.io.rp(0).addr}\n")
  // printf(cf"reg.io.rp(0).data: ${reg.io.rp(0).data}%x\n")
  // printf(cf"reg.io.rp(1).addr: ${reg.io.rp(1).addr}\n")
  // printf(cf"reg.io.rp(1).data: ${reg.io.rp(1).data}%x\n")
  val rs2 = Mux(!hzd.io.bypassRS2RD, reg.io.rp(1).data, lsu.io.rddata)
  //  val rs2 = reg.io.rp(1).data

  reg.io.wp(0).bits.addr := ctrlSig.rd
  reg.io.wp(1).bits.addr := me_rdaddr
  reg.io.wp(0).valid := ctrlSig.w0Wb.asUInt.orR && !stall
  //  reg.io.wp(0).valid := ctrlSig.w0Wb.asUInt.orR
  // FIXME: Decoupling
  reg.io.wp(1).valid := me_isLoad.asUInt.orR && !stallME
  //  reg.io.wp(1).valid := me_isLoad.asUInt.orR
  // printf(cf"reg.io.wp(0).valid: ${reg.io.wp(0).valid}\n")
  // printf(cf"alu.io.R: ${alu.io.R}%x\n")

  reg.io.wp(0).bits.data := Mux1H(Seq(
    RdType.Alu -> alu.io.R,
    //    RdType.ConsecPC -> (frontend.io.if_pc + 4.U),
    //    RdType.ConsecPC -> (frontend.io.if_pc),
    RdType.ConsecPC -> (frontend.io.ie_pc + 4.U),
    RdType.BypassCSR -> csr.io.rd
  ).map {case(k, v) => (k === ctrlSig.rdType, v)})
  reg.io.wp(1).bits.data := lsu.io.rddata


  // ALU
  alu.io.ctrl := ctrlSig.aluCtrl

  alu.io.A := Mux1H(Seq(
    OperandType.None -> 0.U,
    OperandType.Reg -> rs1,
    OperandType.PC -> ie_pc,
    OperandType.CSRImmediate -> ctrlSig.rs1 // rs1 field of instruction is imm field
  ).map { case (k, v) => (k === ctrlSig.operandSelect.a, v) })

  alu.io.B := Mux1H(Seq(
    OperandType.None -> 0.U,
    OperandType.Reg -> rs2,
    OperandType.IImmediate -> ctrlSig.imm.i.asUInt,
    OperandType.UImmediate -> ctrlSig.imm.u.asUInt,
    OperandType.JImmediate -> ctrlSig.imm.j.asUInt,
    OperandType.SImmediate -> ctrlSig.imm.s.asUInt
  ).map { case (k, v) => (k === ctrlSig.operandSelect.b, v) })
  // printf(cf"ctrlSig.operandSelect.a: ${ctrlSig.operandSelect.a}\n")
  // printf(cf"ctrlSig.operandSelect.b: ${ctrlSig.operandSelect.b}\n")
  // printf(cf"alu.io.ctrl: ${alu.io.ctrl}\n")
  // printf(cf"alu.io.A: ${alu.io.A}%x\n")
  // printf(cf"alu.io.B: ${alu.io.B}%x\n")



  // CSR
  csr.io.ctrl.inst := ctrlSig.csrCtrl
  csr.io.ctrl.in := alu.io.R
  csr.io.ctrl.addr := ie_inst(31, 20)

  csr.io.hartId := hartId.U
  csr.io.ecall := ctrlSig.ecall
  csr.io.ebreak := ctrlSig.ebreak
  csr.io.mret := ctrlSig.mret
  csr.io.wfi := ctrlSig.wfi

  // FIXME: When speculative load/store supported, exception should be failed load/store pc
//  csr.io.exception := meXcpt
//  csr.io.cause := meCause
  csr.io.exception := ieXcpt
  csr.io.cause := ieCause
  csr.io.epc := me_pc
  csr.io.loadAddr := me_aluR

  io.powerdown := csr.io.wfiOut

  // interrupt
  csr.io.interrupt := io.interrupt


  // Hazard
  // FIXME: Why does this not work?
  hzd.io.rs1Valid := ((OperandType.Reg) === (ctrlSig.operandSelect.a))
  // RS2: ALU.B || Store || Branch
  hzd.io.rs2Valid := ((ctrlSig.operandSelect.b) === (OperandType.Reg)) ||
    ((ctrlSig.frontendCtrl) === (FrontendControlIE.BR)) ||
    ((ctrlSig.lsuCtrl.isStore) === (StoreControl.EN))
  hzd.io.loadValid := me_isLoad === LoadControl.EN

  hzd.io.rs1Addr := ctrlSig.rs1
  hzd.io.rs2Addr := ctrlSig.rs2
  hzd.io.rdAddrME := me_rdaddr
  hzd.io.rdAddrIE := ctrlSig.rd
  lsu.io.ldKill := hzd.io.ldKill

  // LSU
  lsu.io.lsuctrlIE <> ctrlSig.lsuCtrl
  lsu.io.lsuctrlME <> me_lsu

  lsu.io.edm <> io.edm
  lsu.io.addr := alu.io.R
  lsu.io.wrdata := rs2
  // printf(cf"lsu.io.addr: ${lsu.io.addr}%x\n")
  // printf(cf"lsu.io.wrdata: ${lsu.io.wrdata}%x\n")
  lsu.io.stall := stall
  lsu.io.stallME := stallME

  def checkExceptions(x: Seq[(Bool, UInt)]) =
    (x.map(_._1).reduce(_||_), PriorityMux(x))
}
