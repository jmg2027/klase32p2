package klase32

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.util.BitPat.bitPatToUInt
import klase32.config._
import klase32.param.KlasE32ParamKey
import snitch.enums.{OperandType, RdType}

class KlasE32IO(implicit p: Parameters) extends Bundle with KlasE32IOEtc {
  val k = p(KlasE32ParamKey)

  val acc = new Acc.Interface
  val irq = Input(new Interrupt)
  val edm = new EdmIntf
  val epm = new EpmIntf
  val jtag = new JtagIntf
  val dbg = new DbgIntf
}

trait KlasE32IOEtc extends Bundle {
  val resetValue = Input(UInt(32.W))
  val powerdown = Output(Bool())
}

class KlasE32(hartId: Int)(implicit p: Parameters) extends CoreModule
with HasCoreParameters {
  val NOP = BitPat("b00000000000000000000000000010011")

  val io = IO(new KlasE32IO)

  val alu = Module(new ALU)
  val lsu = Module(new LSU)
  val div = Module(new DIV)
  val mpy = Module(new MPY)

  val frontend = Module(new Frontend)
  val csr = Module(new CSRModule)
  val hzd = Module(new Hazards)
  val dbgCtrl = Module(new DebugController)
  val jtag = Module(new JtagInterface)
  val dec = Module(new Decoder)
  val reg = Module(new RegisterFile())

  // Registers
  val bootAddrWire = WireInit(bootAddrParam.U)
  if (usingOuterBoodAddr) { val bootAddrWire = io.epm.bootAddr}
  val pcReg = RegEnable(frontend.io.pcRegWrite.bits, bootAddrWire, frontend.io.pcRegWrite.valid && !stallSig.proc)

  // Control signal
  val ctrlSig = dec.io.decSig.bits

  // interrupt
  csr.io.irq := io.irq
  csr.io.br := frontend.io.br

  // Stall
  val stallSig = new Stall
  // stallSig.proc := io.waitDmReq || io.waitPmPf
  stallSig.proc := frontend.io.procStall // FIX THIS
  stallSig.hzd := hzd.io.stall
  // stallSig.IE := ctrlSig.fence && (io.edm.ld_ack || io.edm.st_ack)
  stallSig.IE := frontend.io.stallIE // FIX THIS
  val stall = stallSig.asUInt.orR

  // Pipeline
  val ie_inst = RegEnable(frontend.io.inst.bits, bitPatToUInt(NOP), frontend.io.inst.valid && !stall)
  val ie_pc = RegEnable(pcReg, frontend.io.issue && !stall)

  val me_inst = RegEnable(ie_inst, bitPatToUInt(NOP), !stall)
  val me_lsu = RegEnable(ctrlSig.lsuCtrl, !stall)
  val me_reg = RegEnable(ctrlSig.lsuCtrl.isLoad, !stall)
  val me_hzd = RegEnable(ctrlSig.hzdLoadCtrl, !stall)

  // Fetch & Issue
  io.epm <> frontend.io.epm

  frontend.io.ocdExe := DontCare
  frontend.io.ocdInst := DontCare
  frontend.io.ocdReq := DontCare

  frontend.io.divBusy := DontCare

  frontend.io.if_pc := pcReg
  frontend.io.cnd := alu.io.F
  frontend.io.dmAck := io.edm.ld_ack || io.edm.st_ack
  frontend.io.hzdStall := hzd.io.stall

  // frontend.io.of13 := ctrlSig.imm.b
  // frontend.io.of21 := ctrlSig.imm.j
  // frontend.io.jalrTrgt := alu.io.R
  frontend.io.aluR := alu.io.R

  // frontend.io.ie_pc := ie_pc

  // Decode
  dec.io.inst := ie_inst

  // distribute control signals
  frontend.io.ctrl := ctrlSig.pcCtrl
  frontend.io.fence := ctrlSig.fence
  alu.io.ctrl := ctrlSig.aluCtrl
  lsu.io.lsuctrlIE <> ctrlSig.lsuCtrl
  hzd.io.hzdCtrl := ctrlSig.hzdCtrl
  reg.io.wp(0).valid := ctrlSig.w1Wb.asUInt.asBool && !stall
  csr.io.ctrl.inst := ctrlSig.csrCtrl
  frontend.io.flushEn := ctrlSig.flushICache
  // div.io.ctrl := dec.io.IECtrl.div
  // mpy.io.ctrl := dec.io.IECtrl.mpy

  lsu.io.lsuctrlME <> me_lsu
  reg.io.wp(1).valid := me_reg.asUInt.asBool && !stallSig.proc
  hzd.io.hzdLoadCtrl := me_hzd

  // ALU
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

  // Register file
  reg.io.rp(0).addr := ctrlSig.rs1
  reg.io.rp(1).addr := ctrlSig.rs2
  val rs1 = reg.io.rp(0).data
  val rs2 = reg.io.rp(1).data

  reg.io.wp(0).bits.addr := ctrlSig.rd
  reg.io.wp(1).bits.addr := ctrlSig.rd

  reg.io.wp(0).bits.data := Mux1H(Seq(
    RdType.Alu -> alu.io.R,
    RdType.ConsecPC -> (pcReg + 4.U),
    RdType.BypassCSR -> csr.io.rd
  ).map {case(k, v) => (k === ctrlSig.rdType, v)})
  reg.io.wp(1).bits.data := lsu.io.rddata

  // LSU
  lsu.io.edm <> io.edm
  lsu.io.addr := alu.io.R
  lsu.io.wrdata := rs2
}
