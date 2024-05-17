package klase32

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import klase32.config._
import klase32.param.KLASE32ParamKey

class FrontendIO(implicit p: Parameters) extends CoreBundle with HasCoreParameters {
  val ctrl = Input(CtrlControlIE())

  val if_pc = Input(UInt(mxLen.W)) // Current PC
  // val ie_pc = Input(UInt(mxLen.W)) // 1 cycle before PC
  val evec = Input(UInt(mxLen.W))
  val cnd = Input(Bool())
  val exception = Input(Bool())
  val eret = Input(Bool())

  val divBusy = Input(Bool())
  val stall = Input(Bool())

  val aluR = Input(UInt(mxLen.W))

  val issue = Output(Bool())

  val pcRegWrite = Output(Valid(UInt(mxLen.W))) // Next PC

  val instPacket = Output(new Bundle {
    val inst = UInt(wordsize.W)
    val xcpt = new HeartXcpt
  }) // IF stage

  val flushEn = Input(IcacheFlushIE())

  val epm = new EpmIntf
}


class Frontend(implicit p: Parameters) extends CoreModule {
  import CtrlControlIE._
  val k = p(KLASE32ParamKey)

  val io = IO(new FrontendIO())

  io.epm.kill := DontCare

  val brCond = (io.ctrl === BR) & io.cnd
  val haltCond = (io.ctrl === HALT)
  val jalCond = (io.ctrl === JAL)
  val jalrCond = (io.ctrl === JALR) | (io.ctrl === MRET)

  val bootingUpdate = Wire(Bool())
  val booting = Wire(Bool())
  val regBooting = RegEnable(booting && !io.stall, 1.B, bootingUpdate)

  bootingUpdate := false.B
  booting := false.B
  when(regBooting) {
    booting := true.B
    bootingUpdate := false.B
  }
  // Fetch Queue
  // FIXME: Support for C extension
  // For now 32-bit N entries
  // To extend to compressed mode, fetch queue should handle 16-bit data
  // when empty, enq data will be dequeued instantly
  val fq = Module(new Queue(new FetchQueueEntry, fetchqueueEntries, flow = true, hasFlush = true))
  fq.io.enq.bits.data := io.epm.data
  fq.io.enq.bits.xcpt := io.epm.xcpt
  fq.io.enq.valid := io.epm.ack // Suppose simultaneous ack and data response

  // Why this generates nullpointerexception?
  //  def isRVC(inst: UInt): Bool = !(inst(1) && inst(0))

  // Issue
  val issueable = !io.stall && !io.divBusy
  /* FIXME: RVC
  Fetch queue will fetch fetchwidth. fetchqueue should store 2 bytes align
  Extended instruction will be issued: 32 bits
   */
  val instrAvail = fq.io.deq.valid && !regBooting
  // When fq being flushed, NOP is issued(1 cycle stall)
  val issue = instrAvail && issueable

  val instIF = fq.io.deq.bits.data
  val xcptIF = fq.io.deq.bits.xcpt

  // Next PC
  val brIE = brCond && !io.stall
  val jalIE = jalCond && !io.stall
  val jalrIE = jalrCond && !io.stall
  val jump = brIE || jalIE || jalrIE || regBooting

  val jumpPC = Mux(brIE || jalIE || jalrIE, io.aluR, io.if_pc)

  val pcWrite = Mux(jump, jumpPC, io.if_pc + Mux(!(instIF(1) && instIF(0)), 2.U, 4.U))

  when(jump || issue) {
    io.pcRegWrite.valid := true.B
    io.pcRegWrite.bits := pcWrite
  } otherwise {
    io.pcRegWrite.valid := false.B
    io.pcRegWrite.bits := 0.U
  }

  // Fetch PC

  val issueLength = Mux((!(io.epm.data(0) && io.epm.data(1))), 2.U, 4.U)
  val fetch = fq.io.enq.ready

  val bootAddrWire = WireDefault(bootAddrParam.U)
  if (usingOuterBoodAddr) {
    val bootAddrWire = io.epm.bootAddr
  }
  //  val fetchPC = RegInit(bootAddrWire.asUInt, UInt(mxLen.W))
  // FIXME: MUX
  val fetchPC = Reg(UInt(mxLen.W))
  when (reset.asBool) {
    fetchPC := bootAddrWire
    fq.flush := true.B
  }.elsewhen(io.exception || io.eret) {
    fetchPC := io.evec
    fq.flush := true.B
  }.elsewhen (jump) {
    fetchPC := jumpPC
    fq.flush := true.B
  }.otherwise {
    fetchPC := fetchPC + issueLength
    fq.flush := false.B
  }

  io.epm.addr := Mux(fetch, fetchPC, 0.U)
  io.epm.req := fetch
  io.epm.flush := io.flushEn.asUInt.orR
  io.epm.cmd := 0.U // int load

  // Issue
  fq.io.deq.ready := issue // issue signal will block when stalled
  io.instPacket.inst := Mux(issue, instIF, 0.U)
  io.instPacket.xcpt := Mux(issue, xcptIF, 0.U.asTypeOf(new HeartXcpt))
  io.issue := issue


}
