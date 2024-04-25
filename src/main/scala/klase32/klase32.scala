package klase32

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.util.BitPat.bitPatToUInt
import klase32.config._
import klase32.param.KlasE32ParamKey
import snitch.enums.{OperandType, RdType}
import freechips.rocketchip.rocket.Causes

class KlasE32IO(implicit p: Parameters) extends Bundle with KlasE32IOEtc {
  val k = p(KlasE32ParamKey)

//  val acc = new Acc.Interface
  val interrupt = Input(new Interrupt)
  val edm = new EdmIntf
  val epm = new EpmIntf
//  val jtag = new JtagIntf
//  val dbg = new DbgIntf
}

trait KlasE32IOEtc extends Bundle {
  val resetValue = Input(UInt(32.W))
  val powerdown = Output(Bool())
}

class KlasE32(hartId: Int)(implicit p: Parameters) extends CoreModule
  with HasCoreParameters {
  val NOP = BitPat("b00000000000000000000000000010011")

  val io = IO(new KlasE32IO)

  // FIXME: WFI
  io.powerdown := DontCare

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
  // FIXME: LSQ
  when (io.edm.ld_req) {
    stallSig.me.load := !io.edm.ld_ack
  }
  when (io.edm.st_req) {
    stallSig.ie.store := !io.edm.st_ack
  }
  stallSig.me.hzd := hzd.io.stall
  stallSig.me.fence := ctrlSig.fence.asUInt.orR && (io.edm.ld_ack || io.edm.st_ack)
  val stallIE = stallSig.ie.asUInt.orR
  val stallME = stallSig.me.asUInt.orR
  val stall = stallIE && stallME


  // PC Register
  val bootAddrWire = WireInit(bootAddrParam.U)
  if (usingOuterBoodAddr) { val bootAddrWire = io.epm.bootAddr}
  val pcReg = RegEnable(frontend.io.pcRegWrite.bits, bootAddrWire, frontend.io.pcRegWrite.valid && !stall)


  // Pipeline
  val ie_inst = RegEnable(frontend.io.instPacket.bits.inst, bitPatToUInt(NOP), frontend.io.instPacket.valid && !stall)
  val ie_pc = RegEnable(pcReg, frontend.io.issue && !stall)

  val me_inst = RegEnable(ie_inst, bitPatToUInt(NOP), !stallME)
  val me_lsu = RegEnable(ctrlSig.lsuCtrl, !stallME)
  val me_isLoad = RegEnable(ctrlSig.lsuCtrl.isLoad, !stallME)
  val me_rdaddr = RegEnable(ctrlSig.rd, !stallME)


  // Exception
  val (ieXcpt, ieCause) = checkExceptions(List(
//    (csr.io.interruptPending, csr.io.interruptCause),
    (frontend.io.instPacket.bits.xcpt.ma, Causes.misaligned_fetch.U),
    (frontend.io.instPacket.bits.xcpt.pf, Causes.fetch_page_fault.U),
    (frontend.io.instPacket.bits.xcpt.gf, Causes.fetch_guest_page_fault.U),
    (frontend.io.instPacket.bits.xcpt.ae, Causes.fetch_access.U),

    (io.edm.st_ack && io.edm.xcpt.ma, Causes.misaligned_store.U),
    (io.edm.st_ack && io.edm.xcpt.pf, Causes.store_page_fault.U),
    (io.edm.st_ack && io.edm.xcpt.gf, Causes.store_guest_page_fault.U),
    (io.edm.st_ack && io.edm.xcpt.ae, Causes.store_access.U),
    (ctrlSig.illegal === IllegalInstIE.EN, Causes.illegal_instruction.U),
    (ctrlSig.ecall === EcallIE.EN, Causes.machine_ecall.U),
    (ctrlSig.ebreak === EbreakIE.EN, Causes.breakpoint.U),
  ))

  // Exception pipeline
  val ieXcptReg = RegEnable(ieXcpt, !stall)
  val ieCauseReg = RegEnable(ieCause, !stall)

  val (meXcpt, meCause) = checkExceptions(List(
    (ieXcptReg, ieCauseReg),
    (io.edm.ld_ack && io.edm.xcpt.ma, Causes.misaligned_load.U),
    (io.edm.ld_ack && io.edm.xcpt.pf, Causes.load_page_fault.U),
    (io.edm.ld_ack && io.edm.xcpt.gf, Causes.load_guest_page_fault.U),
    (io.edm.ld_ack && io.edm.xcpt.ae, Causes.load_access.U),
  ))


  csr.io.exception := meXcpt
  csr.io.cause := meCause

  // Fetch & Issue
  io.epm <> frontend.io.epm
  io.epm.kill := DontCare


  // ctrl
  frontend.io.ctrl := ctrlSig.pcCtrl
  frontend.io.flushEn := ctrlSig.flushICache

  frontend.io.divBusy := DontCare

  frontend.io.if_pc := pcReg
  frontend.io.evec := csr.io.out.mtvec.asUInt
  frontend.io.cnd := alu.io.F
  frontend.io.exception := meXcpt
  frontend.io.eret := ctrlSig.ecall.asUInt.orR || ctrlSig.ebreak.asUInt.orR || ctrlSig.mret.asUInt.orR

//  frontend.io.dmAck := io.edm.ld_ack || io.edm.st_ack
  frontend.io.stall := stall

  frontend.io.aluR := alu.io.R


  // Decode
  dec.io.inst := ie_inst


  // Register file
  reg.io.rp(0).addr := ctrlSig.rs1
  reg.io.rp(1).addr := ctrlSig.rs2
  val rs1 = reg.io.rp(0).data
  val rs2 = reg.io.rp(1).data

  reg.io.wp(0).bits.addr := ctrlSig.rd
  reg.io.wp(1).bits.addr := ctrlSig.rd
  reg.io.wp(0).valid := ctrlSig.w1Wb.asUInt.orR && !stall
  reg.io.wp(1).valid := me_isLoad.asUInt.orR && !stallME

  reg.io.wp(0).bits.data := Mux1H(Seq(
    RdType.Alu -> alu.io.R,
    RdType.ConsecPC -> (pcReg + 4.U),
    RdType.BypassCSR -> csr.io.rd
  ).map {case(k, v) => (k === ctrlSig.rdType, v)})
  reg.io.wp(1).bits.data := lsu.io.rddata


  // ALU
  alu.io.ctrl := ctrlSig.aluCtrl

  alu.io.A := Mux1H(Seq(
    OperandType.None -> 0.U,
    OperandType.Reg -> rs1,
    OperandType.PC -> ie_pc,
    OperandType.CSRImmmediate -> ctrlSig.rs1 // rs1 field of instruction is imm field
  ).map { case (k, v) => (k === ctrlSig.operandSelect.a, v) })
  alu.io.B := Mux1H(Seq(
    OperandType.None -> 0.U,
    OperandType.Reg -> rs2,
    OperandType.IImmediate -> ctrlSig.imm.i.asUInt,
    OperandType.SImmediate -> ctrlSig.imm.s.asUInt
  ).map { case (k, v) => (k === ctrlSig.operandSelect.a, v) })




  // CSR
  csr.io.ctrl.inst := ctrlSig.csrCtrl
  csr.io.ctrl.in := alu.io.R
  csr.io.ctrl.addr := ie_inst(31, 20)

  csr.io.hartId := hartId.U
  csr.io.ecall := ctrlSig.ecall
  csr.io.ebreak := ctrlSig.ebreak
  csr.io.wfi := ctrlSig.wfi

  // interrupt
  csr.io.interrupt := io.interrupt


  // Hazard
  hzd.io.rs1Valid := ctrlSig.operandSelect.a === OperandType.Reg
  hzd.io.rs2Valid := ctrlSig.operandSelect.b === OperandType.Reg
  hzd.io.loadValid := me_isLoad === LoadControl.EN

  hzd.io.rs1Addr := ctrlSig.rs1
  hzd.io.rs2Addr := ctrlSig.rs2
  hzd.io.rdAddr := me_rdaddr


  // LSU
  lsu.io.lsuctrlIE <> ctrlSig.lsuCtrl
  lsu.io.lsuctrlME <> me_lsu

  lsu.io.edm <> io.edm
  lsu.io.addr := alu.io.R
  lsu.io.wrdata := rs2

  def checkExceptions(x: Seq[(Bool, UInt)]) =
    (x.map(_._1).reduce(_||_), PriorityMux(x))
}
