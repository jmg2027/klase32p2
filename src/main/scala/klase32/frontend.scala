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
  val ie_pc = Input(UInt(mxLen.W))
  val brOffset = Input(UInt(13.W))

  val issue = Output(Bool())

  val instPacket = Output(new Bundle {
    val inst = UInt(wordsize.W)
    val xcpt = new HeartXcpt
  }) // IF stage

  val flushIcache = Input(IcacheFlushIE())
  val flushPipeline = Output(Bool())

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

  val bootAddrWire = WireDefault(bootAddrParam.U)
  if (usingOuterBoodAddr) {
    bootAddrWire := io.epm.bootAddr
  }

  // Next PC
  val brIE = brCond && !io.stall
  val jalIE = jalCond && !io.stall
  val jalrIE = jalrCond && !io.stall
  //  val jump = brIE || jalIE || jalrIE || regBooting
  val jump = brIE || jalIE || jalrIE
  io.flushPipeline := jump

  val brAddr = io.ie_pc + io.brOffset
  //  val jumpPC = Mux(brIE, (io.ie_pc + io.brOffset), io.aluR)
  val jumpPC = Mux(brIE, brAddr, io.aluR)

  // Fetch Queue
  // FIXME: Support for C extension
  // For now 32-bit N entries
  // To extend to compressed mode, fetch queue should handle 16-bit data
  // when empty, enq data will be dequeued instantly
  // FIXME: Check for flow in jump
//   val fq = Module(new Queue(new FetchQueueEntry, fetchqueueEntries, flow = true, hasFlush = true))
  val fq = Module(new Queue(new FetchQueueEntry, fetchqueueEntries, flow = false, hasFlush = true))

  // Fetch
//  val fetch = WireDefault(false.B)

  // Fetch counter
  val fqCounter = withReset(reset.asBool || fq.io.flush.getOrElse(false.B)) { RegInit((fetchqueueEntries - 1).U) }

  when (fq.io.enq.fire && fq.io.deq.fire) {
    fqCounter := fqCounter
  }.elsewhen (fq.io.enq.fire) {
    fqCounter := fqCounter - 1.U
  }.elsewhen (fq.io.deq.fire) {
    fqCounter := fqCounter + 1.U
  }
  val fetchAvail = fqCounter =/= 0.U

  // When exception happens, only 1 request can go out
  val xcptReqOntheway = RegInit(false.B)
  when (io.exception && !regBooting) {
    xcptReqOntheway := true.B
  }
  when(io.epm.ack && xcptReqOntheway) {
      xcptReqOntheway := false.B
  }


  val fetch = fetchAvail && !regBooting && !xcptReqOntheway
  val fetchPC = Reg(UInt(mxLen.W))
  printf(cf"[FE] exception: ${io.exception}\n")
  printf(cf"[FE] evec: ${io.evec}\n")
  when(regBooting) {
//    fetch := false.B
    fetchPC := bootAddrWire
    fq.flush := false.B
  }.elsewhen(io.exception || io.eret) {
//    fetch := fq.io.enq.ready
//    fetch := true.B
    fetchPC := io.evec
    fq.flush := true.B
  }.elsewhen(jump) {
//    fetch := fq.io.enq.ready
//    fetch := true.B
    fetchPC := jumpPC
    fq.flush := true.B
  }.elsewhen(fetch) {
    fetchPC := fetchPC + 4.U
    fq.flush := false.B
  }.otherwise {
    fetchPC := fetchPC
    fq.flush := false.B
  }
//}.elsewhen(!fetch) {
////    fetch := false.B
//    fetchPC := fetchPC
//    fq.flush := false.B
//    // FIXME: Does this not needed?
//  }.otherwise {
////    fetch := fq.io.enq.ready
////    fetch := true.B
//    fetchPC := fetchPC + 4.U
//    fq.flush := false.B
//  }

  // When flush, ignore acks for on-the-fly reqs
  // All requests recieve ack so overflow should not happen
  // Flush can occur only after when enq.valid is enable, except for async exception
  // So maximum number of on-the-fly requests are 2 times of fetchqueueEntries
  // For exception(async exception), only 1 request will comes out
  val reqCounter = RegInit(0.U(log2Ceil(fetchqueueEntries * 2 - 1).W))
  val ignoreAck = RegInit(false.B)
  when(fq.io.flush.getOrElse(false.B)) {
    reqCounter := reqCounter + (fetchqueueEntries - 1).U - fqCounter
    ignoreAck := true.B
  }

  when(ignoreAck && io.epm.ack) {
    reqCounter := reqCounter - 1.U
  }

//  when(reqCounter === 0.U && ignoreAck) {
  when(reqCounter === 1.U && ignoreAck && io.epm.ack) {
    ignoreAck := false.B
  }

  io.epm.addr := fetchPC
  // printf(cf"[FE] fetchPC: 0x${io.epm.addr}%x\n")
  //  io.epm.req := fetch
  io.epm.req := fetch
  io.epm.flush := io.flushIcache.asUInt.orR
  io.epm.cmd := 0.U // int load

  fq.io.enq.bits.data := io.epm.data
  fq.io.enq.bits.xcpt := io.epm.xcpt
  fq.io.enq.valid := io.epm.ack && !ignoreAck // Suppose simultaneous ack and data response


  // Why this generates nullpointerexception?
  //  def isRVC(inst: UInt): Bool = !(inst(1) && inst(0))

  // Issue
  val issueable = !io.divBusy
  /* FIXME: RVC
  Fetch queue will fetch fetchwidth. fetchqueue should store 2 bytes align
  Extended instruction will be issued: 32 bits
   */
  val instrAvail = fq.io.deq.valid && !regBooting && !ignoreAck
  //  val instrAvail = fq.io.deq.fire && !regBooting
  // When fq being flushed, NOP is issued(1 cycle stall)
  val issue = instrAvail && issueable

  val instIF = fq.io.deq.bits.data
  val xcptIF = fq.io.deq.bits.xcpt


  val pcWrite = Wire(UInt())

  // PC Register
  // Address of current instruction in IF stage
  val pcReg = RegEnable(pcWrite, bootAddrWire, (jump || issue) && !io.stall)
  io.if_pc := pcReg
   printf(cf"[FE] pc: 0x$pcReg%x\n")
   printf(cf"pcw: 0x$pcWrite%x\n")
  // printf(cf"[FE] jump: $jump\n")
  // printf(cf"[FE] issue: $issue\n")
  // printf(cf"boot: $regBooting\n")
  // printf(cf"fq: ${fq.io.deq.valid}\n")
  // printf(cf"stall: ${io.stall}\n")

  pcWrite := Mux(jump, jumpPC, pcReg + 4.U)



  // Issue
  fq.io.deq.ready := !io.stall // issue signal will block when stalled
  //  fq.io.deq.ready := true.B
  //  io.instPacket.inst := Mux(issue, instIF, 0.U)
  //  io.instPacket.xcpt := Mux(issue, xcptIF, 0.U.asTypeOf(new HeartXcpt))
  io.instPacket.inst := instIF
  io.instPacket.xcpt := xcptIF
  io.issue := issue
}
