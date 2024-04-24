package klase32

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import klase32.config._
import klase32.param.KlasE32ParamKey

class FrontendIO(implicit p: Parameters) extends CoreBundle with HasCoreParameters {
  val ctrl = Input(CtrlControlIE())

  val if_pc = Input(UInt(mxLen.W)) // Current PC
  // val ie_pc = Input(UInt(mxLen.W)) // 1 cycle before PC
  val evec = Input(UInt(mxLen.W))
  val br = Output(Bool()) // FIX THIS
  val cnd = Input(Bool())
  val xcpt = Input(Bool())
  val eret = Input(Bool())

  val divBusy = Input(Bool())
  val hzdStall = Input(Bool())

  val ocdExe = Input(Bool())
  val ocdInst = Input(UInt(wordsize.W))
  val ocdReq = Input(Bool())
  val ocdAck = Output(Bool())

  val aluR = Input(UInt(mxLen.W))

  val waitDmReq = Input(Bool())
  val waitPmPf = Input(Bool())

  val issue = Output(Bool())

  val pcRegWrite = Output(Valid(UInt(mxLen.W))) // Next PC

  val inst = Output(Valid(UInt(wordsize.W))) // IF stage

  val flushEn = Input(Bool())

  val epm = new EpmIntf
}


class Frontend(implicit p: Parameters) extends CoreModule {
  import CtrlControlIE._
  val k = p(KlasE32ParamKey)

  val io = IO(new FrontendIO())

  io.br := (io.ctrl === BR)
  val brCond = io.br & io.cnd
  val haltCond = (io.ctrl === HALT)
  val jalCond = (io.ctrl === JAL)
  val jalrCond = (io.ctrl === JALR) | (io.ctrl === MRET)

  val bootingUpdate = Wire(Bool())
  val booting = Wire(Bool())
  val regBooting = RegEnable(booting && !io.procStall, 1.B, bootingUpdate)

  bootingUpdate := false.B
  booting := false.B
  when(regBooting) {
    booting := true.B
    bootingUpdate := false.B
  }

  val io.ocdAck = debugmode || regDebugmode || regDebugack || !instrAvail
  val regDebugack = RegEnable(!io.procStall, io.ocdAck)

  val debugmode = io.ocdReq && (issueable || regDebugmode)
  val regDebugmode = RegEnable(!io.procStall, debugmode)

  // Issue
  val issueable = !io.hzdStall && !io.divBusy
  // FIXME: RVC
  // val instrAvail = fq.io.deq.valid && (!io.pmPartial || isRVC(instIF)) && !regBooting
  val instrAvail = fq.io.deq.valid && !regBooting
  // When fq being flushed, NOP is issued(1 cycle stall)
  val issue = instrAvail && issueable && !(debugmode || regDebugmode)
  // val issue = instrAvail && !brCond && issueable && !(debugmode || regDebugmode)
  io.inst.bits := Mux(io.ocdExe, io.ocdInst, Mux(issue, instIF, 0.U))
  io.inst.valid := io.ocdExe || issue
  io.issue := issue

  // Next PC
  val leaveDebugmode = !debugmode && regDebugmode
  val brIE = brCond && !io.hzdStall
  val jalIE = jalCond && !io.hzdStall & !brIE
  val jalrIE = jalrCond && !io.hzdStall
  val jump = brIE || jalIE || jalrIE || regBooting || leaveDebugmode

  val jumpPC = Mux(brIE || jalIE || jalrIE, io.aluR, io.if_pc)

  val pcWrite = Mux(jump, jumpPC, io.if_pc + Mux(fq.io.deq.bits.rvc, 2.U, 4.U))



  when(jump || issue) {
    io.pcRegWrite.valid := true.B
    io.pcRegWrite.bits := pcWrite
  } otherwise {
    io.pcRegWrite.valid := false.B
    io.pcRegWrite.bits := 0.U
  }

  // Fetch Queue
  // FIXME: Support for C extension
  // For now 32-bit N entries
  // To extend to compressed mode, fetch queue should handle 16-bit data
  // when empty, enq data will be dequeued instantly
  val fq = Module(new Queue(new FetchQueueIntf, fetchqueueEntries, flow = true, hasFlush = true))
  fq.io.enq.bits.data := io.epm.data
  fq.io.enq.bits.xcpt := io.epm.xcpt
  fq.io.enq.bits.rvc := isRVC(io.epm.data)
  fq.io.enq.valid := io.epm.ack // Suppose simultaneous ack and data response

  val instIF = fq.io.deq.bits.data
  val xcptIF = fq.io.deq.bits.xcpt
  fq.io.deq.ready := issue // issue signal will block when stalled

  // needs flush conditions more such as exception, interrupt, mret, replay
  val fetch = !debugmode || fq.io.enq.ready

  io.epm.addr := Mux(fetch, fetchPC, 0.U)
  io.epm.req := fetch
  io.epm.flush := io.flushEn
  io.epm.cmd := 0.U // int load
  // io.pmJump := jump
  // io.pmTarget := jumpPC

  // Fetch PC
  def isRVC(inst: UInt): Bool = inst(1, 0) !== 3.U
  val issueLength = Mux(isRVC(io.epm.data), 2.U, 4.U)

  val bootAddrWire = WireInit(bootAddrParam.U)
  if (usingOuterBoodAddr) {
    val bootAddrWire = io.epm.bootAddr
  }
  val fetchPC = RegInit(bootAddrWire, UInt(mxLen.W))
  when (jump) {
    fetchPC := jumpPC
    fq.flush := true.B
  } elsewhen (io.xcpt || io.eret) {
    fetchPC := io.evec
    fq.flush := true.B
  } otherwise {
    fetchPC := fetchPC + issueLength
  }


  // FIXME: Extend to compressed mode
}
