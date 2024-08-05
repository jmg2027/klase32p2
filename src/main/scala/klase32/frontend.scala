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
  val wfi = Input(Bool())

  val aluR = Input(UInt(mxLen.W))
  val ie_pc = Input(UInt(mxLen.W))
  val brOffset = Input(UInt(13.W))

  val issue = Output(Bool())

//  val instPacket = Output(new Bundle {
//    val inst = UInt(wordsize.W)
//    val xcpt = new HeartXcpt
//  }) // IF stage
  val instPacket = Decoupled(new InstructionPacket())

  val flushFetchQueue = Input(new Flush())
  val flushIcache = Input(IcacheFlushIE())
  val jump = Output(Bool())

  val epm = new EpmIntf
}


class Frontend(implicit p: Parameters) extends CoreModule {

  import FrontendControlIE._

  val k = p(KLASE32ParamKey)

  val io = IO(new FrontendIO())

  val fq = Module(new Queue(new FetchQueueEntry, fetchqueueEntries, flow = false, hasFlush = true))

  // When kill asserts, all previous requests are blown away
  io.epm.kill := fq.flush

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
  if (usingOuterBootAddr) {
    bootAddrWire := io.epm.bootAddr
  }

  // Next PC
  val brIE = brCond && !io.stall
  val jalIE = jalCond && !io.stall
  val jalrIE = jalrCond && !io.stall
  val jump = brIE || jalIE || jalrIE
  io.jump := jump

  val brAddr = io.ie_pc + io.brOffset
  val jumpPC = Mux(brIE, brAddr, io.aluR)

  // Fetch
  // Fetch Queue
  // FIXME: Check for flow in jump

  // Fetch counter
  val fqCounter = withReset(reset.asBool || fq.flush) { RegInit((fetchqueueEntries - 1).U) }

  when (fq.io.enq.fire && fq.io.deq.fire) {
    fqCounter := fqCounter
  }.elsewhen (fq.io.enq.fire) {
    fqCounter := fqCounter - 1.U
  }.elsewhen (fq.io.deq.fire) {
    fqCounter := fqCounter + 1.U
  }
  val fqNotFull = fqCounter =/= 0.U

  // When exception happens, only 1 request can go out
  val xcptReqOntheway = RegInit(false.B)
  when (io.epm.req && io.exception && !regBooting) {
    xcptReqOntheway := true.B
  }
  when(io.epm.ack && xcptReqOntheway) {
    xcptReqOntheway := false.B
  }

  // Flush fetch queue
  fq.flush := io.flushFetchQueue.ie.xcpt ||
    io.flushFetchQueue.ie.jump ||
    io.flushFetchQueue.ie.eret ||
    io.flushFetchQueue.ie.csr
  // Fence does not flush fetch queue
  // fq.flush := io.flushFetchQueue.orR

  val fetch = fqNotFull && !regBooting && !xcptReqOntheway && !io.wfi

  // Fetch PC
  val fetchPC = Reg(UInt(mxLen.W))

  printf(cf"[FE] exception: ${io.exception}\n")
  printf(cf"[FE] evec: ${io.evec}%x\n")
  when(regBooting) {
    fetchPC := bootAddrWire
  }.elsewhen(io.exception || io.eret) {
    fetchPC := io.evec
  }.elsewhen(jump) {
    fetchPC := jumpPC
  }.elsewhen(io.flushFetchQueue.ie.csr.orR) {
    fetchPC := io.ie_pc + 4.U
  }.elsewhen(fetch) {
    fetchPC := fetchPC + 4.U
  }.otherwise {
    fetchPC := fetchPC
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
  // Flush can occur only after when enq.start is enable, except for async exception
  // So maximum number of on-the-fly requests are 2 times of fetchqueueEntries
  // For exception(async exception), only 1 request will comes out
//  val reqCounter = RegInit(0.U(log2Ceil(fetchqueueEntries * 2 - 1).W))
//  val ignoreAck = RegInit(false.B)
//  val ignoreAckWire = RegInit(false.B)
//  when(io.epm.req && io.epm.ack) {
//    reqCounter := reqCounter
//  }.elsewhen(io.epm.req) {
//    reqCounter := reqCounter + 1.U
//  }.elsewhen(io.epm.ack) {
//    reqCounter := reqCounter - 1.U
//  }
//
//  ignoreAckWire := fq.flush && ignoreAck
//  when(fq.flush) {
//    ignoreAck := ignoreAckWire
//  }.elsewhen(ignoreAck) {
//    ignoreAck := reqCounter =/= 1.U
//  }


//  when(fq.flush && !xcptReqOntheway) {
//    reqCounter := reqCounter + (fetchqueueEntries - 1).U - fqCounter
//    ignoreAck := true.B
//  }
//
//  when(ignoreAck && io.epm.ack) {
//    reqCounter := reqCounter - 1.U
//  }
//
//  //  when(reqCounter === 0.U && ignoreAck) {
//  when(reqCounter === 1.U && ignoreAck && io.epm.ack) {
//    ignoreAck := false.B
//  }

  // 4 bytes align
  io.epm.addr := fetchPC(31, 2) ## 0.U(2.W)
  // printf(cf"[FE] fetchPC: 0x${io.epm.addr}%x\n")
  io.epm.req := fetch
  // FIXME: Should be handled in the future
  io.epm.flush := io.flushIcache.asUInt.orR
  io.epm.cmd := 0.U // int load

  fq.io.enq.bits.data := io.epm.data
  fq.io.enq.bits.xcpt := io.epm.xcpt
//  fq.io.enq.start := io.epm.ack && !ignoreAck && fqNotFull// Suppose simultaneous ack and data response
  // FIXME: We already dealt with fqNotFull with fq requests, so maybe fqNotFull condition is not necessary
  fq.io.enq.valid := io.epm.ack && fqNotFull// Suppose simultaneous ack and data response

  // Issue
  val issueable = !io.divBusy
  /* FIXME: RVC
  Fetch queue will fetch fetchwidth. fetchqueue should store 2 bytes align
  Extended instruction will be issued: 32 bits
   */
//  val instrAvail = fq.io.deq.start && !regBooting && !ignoreAckWire && !ignoreAck
  val instrAvail = fq.io.deq.valid && !regBooting
  // When fq being flushed, NOP is issued(1 cycle stall)
  val issue = instrAvail && issueable && !ignoreAck

//  val instIF = fq.io.deq.bits.data
//  val xcptIF = fq.io.deq.bits.xcpt


  //  val pcWrite = Wire(UInt())
//  val pcWrite = RegInit(bootAddrWire)

  // PC Register
  // Address of current instruction in IF stage
  // val pcReg = RegEnable(pcWrite, bootAddrWire, (jump || issue) && !io.stall)
//  val nextPC = RegInit(bootAddrWire)
//  val pcReg = RegEnable(nextPC, bootAddrWire, issue)
  val pcReg = RegInit(bootAddrWire)
  io.if_pc := pcReg
  printf(cf"[FE] pc: 0x$pcReg%x\n")
  // printf(cf"[FE] jump: $jump\n")
  // printf(cf"[FE] issue: $issue\n")
  // printf(cf"boot: $regBooting\n")
  // printf(cf"fq: ${fq.io.deq.start}\n")
  // printf(cf"stall: ${io.stall}\n")

  val instBuf = withReset(reset.asBool || fq.flush) { Module(new InstructionBuffer()) }
  val issueLength = Mux(instBuf.io.curInstIsRVC, 2.U, 4.U)

  when(issue && !io.stall) {
    when(io.exception || io.eret) {
      pcReg := io.evec
    }.elsewhen(jump) {
      pcReg := jumpPC
    }.elsewhen(io.flushFetchQueue.ie.csr.orR) {
      pcReg := io.ie_pc + 4.U // ie instruction's issue legnth!
    }.otherwise {
      pcReg := pcReg + issueLength
    }
  }
  // pcWrite := Mux(jump, jumpPC, pcReg + 4.U)
//  pcWrite := Mux(fq.flush, fetchPC, pcReg + 4.U)

  // Issue
  fq.io.deq.ready := !io.stall // issue signal will block when stalled
  io.instPacket <> fq.io.deq
  io.issue := issue

  // Instruction buffer

  instBuf.io.fetchQueueTail <> fq.io.deq
  io.instPacket <> instBuf.io.instPacket
  instBuf.io.if_pc := pcReg


}
