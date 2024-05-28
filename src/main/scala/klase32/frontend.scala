package klase32

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import klase32.config._
import klase32.param.KLASE32ParamKey

class FrontendIO(implicit p: Parameters) extends CoreBundle with HasCoreParameters {
  val ctrl = Input(FrontendControlIE())

  val if_pc = Output(UInt(mxLen.W)) // Current PC
  val evec = Input(UInt(mxLen.W))
  val cnd = Input(Bool())
  val exception = Input(Bool())
  val eret = Input(Bool())

  val divBusy = Input(Bool())
  val stall = Input(Bool())

  val aluR = Input(UInt(mxLen.W))

  val issue = Output(Bool())

  val instPacket = Output(new Bundle {
    val inst = UInt(wordsize.W)
    val xcpt = new HeartXcpt
  }) // IF stage

  val flushEn = Input(IcacheFlushIE())

  val epm = new EpmIntf
}


class Frontend(implicit p: Parameters) extends CoreModule {
  import FrontendControlIE._
  val k = p(KLASE32ParamKey)

  val io = IO(new FrontendIO())

  io.epm.kill := DontCare

  val brCond = (io.ctrl === BR) & io.cnd
  val haltCond = (io.ctrl === HALT)
  val jalCond = (io.ctrl === JAL)
  val jalrCond = (io.ctrl === JALR) | (io.ctrl === MRET)

  // After n cycle fetch will start
  val n = 1 // booting cycle
  val bootingCounter = RegInit(0.U(log2Ceil(n).W))
  val regBooting = RegInit(true.B)
  when(reset.asBool) {
    regBooting := true.B
    bootingCounter := 0.U
  }.elsewhen(regBooting && bootingCounter === (n - 1).U) {
    regBooting := false.B
  }.elsewhen(regBooting) {
    bootingCounter := bootingCounter + 1.U
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
//  val jump = brIE || jalIE || jalrIE || regBooting
  val jump = brIE || jalIE || jalrIE

  val jumpPC = io.aluR

  val pcWrite = Wire(UInt())

  val bootAddrWire = WireDefault(bootAddrParam.U)
  if (usingOuterBoodAddr) {
    bootAddrWire := io.epm.bootAddr
  }

  // PC Register
  // Address of current instruction in IF stage
  val pcReg = RegEnable(pcWrite, bootAddrWire, (jump || issue) && !io.stall)
  io.if_pc := pcReg
  printf(cf"pc: $pcReg\n")
  printf(cf"pcw: $pcWrite\n")
  printf(cf"jump: $jump\n")
  printf(cf"issue: $issue\n")
  printf(cf"boot: $regBooting\n")
  printf(cf"fq: ${fq.io.deq.valid}\n")
  printf(cf"stall: ${io.stall}\n")

  pcWrite := Mux(jump, jumpPC, pcReg + 4.U)

  // Fetch PC
  val issueLength = 4.U
  val fetch = WireDefault(false.B)
  fetch := fq.io.enq.ready && !regBooting
  // FIXME: MUX
  val fetchPC = Reg(UInt(mxLen.W))
  when (reset.asBool && regBooting) {
    fetchPC := bootAddrWire
    fq.flush := true.B
  }.elsewhen(io.exception || io.eret) {
    fetchPC := io.evec
    fq.flush := true.B
  }.elsewhen (jump) {
    fetchPC := jumpPC
    fq.flush := true.B
  }.elsewhen(issue) {
    fetchPC := fetchPC + issueLength
    fq.flush := false.B
  }.otherwise {
    fetchPC := fetchPC
    fq.flush := false.B
  }

  io.epm.addr := fetchPC
  printf(cf"${io.epm.addr}\n")
  io.epm.req := fetch
  io.epm.flush := io.flushEn.asUInt.orR
  io.epm.cmd := 0.U // int load

  // Issue
  fq.io.deq.ready := issue // issue signal will block when stalled
  io.instPacket.inst := Mux(issue, instIF, 0.U)
  io.instPacket.xcpt := Mux(issue, xcptIF, 0.U.asTypeOf(new HeartXcpt))
  io.issue := issue
}
