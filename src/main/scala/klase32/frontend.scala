package klase32

import chisel3._
import chisel3.util._
import chisel3.util.BitPat._
import chisel3.experimental.BundleLiterals._
import klase32.include.config._
import klase32.include.param.KLASE32ParamKey
import klase32.include.ControlSignal._
import klase32.include.KLASE32AbstractClass._
import klase32.include.{EpmIntf, FetchQueueEntry, Flush, HeartXcpt, IFIEPipelineEntry, InstructionPacket}
import freechips.rocketchip.rocket.RVCDecoder
import klase32.functionalunit.{CSRDebug, CSRResetHartRequest, CSRSystemInstruction}

class FrontendIO(implicit p: Parameters) extends CoreBundle with HasCoreParameters {
  val ctrl = Input(FrontendControlIE())

  val evec = Input(UInt(mxLen.W))
  val cnd = Input(Bool())
  val exception = Input(Bool())
  val eret = Input(Bool())
  // for debugger
  val debug = Input(new CSRDebug())
  // TODO: MAKE BUNDLE, SYSTEM inst, RET inst, WFI inst
  val ebreak = Input(Bool())
  val dret = Input(Bool())

  val wfiOut = Input(Bool())
  val wfi = Input(Bool())

  val hartEn = Input(Bool())

  val aluR = Input(UInt(mxLen.W))
  val ie_pc = Input(UInt(mxLen.W))
  val brOffset = Input(UInt(13.W))

  val issue = Decoupled(new IFIEPipelineEntry())
  val instRaw = Output(UInt(32.W))

  val flushFetchQueue = Input(new Flush())
  val flushIcache = Input(IcacheFlushIE())
  val jump = Output(Bool())

  val epm = new EpmIntf

  val resetReq = Output(new CSRResetHartRequest())
}


// It can be optimized.
// You can fit entry size rather than fix it to 8 for log2 value.
// And you can decrease entry size by share allocate space and dequeue space.
// Maybe You can set entry size to 5(prefetchReq size * 2 + 1) for 2 mo after optimization.
class FetchQueue(entries: Int = 8, reqSize: Int = 2) extends Module {
  val indexWidth = log2Ceil(entries)
  val reqSizeBit = log2Ceil(reqSize)
  val mask = entries - 1

  val io = IO(new Bundle {
    val flush = Input(Bool())
    val allocate = Input(Bool())
    val allocSize = Input(UInt(reqSizeBit.W)) // Actual size is allocSize + 1
    val allocateSucceed = Output(Bool())

    val enq = Input(Bool())
    val enqData = Input(UInt((reqSize * 16).W))
    val enqSize = Input(UInt(reqSizeBit.W)) // Actual size is enqSize + 1
    val enqXcpt = Input(new HeartXcpt())

    val canDeq = Output(Bool())
    val deq = Input(Bool())
    val deqData = Output(UInt((reqSize * 16).W))
    val deqXcpt = Output(new HeartXcpt())

    val hasXcpt = Output(Bool())
    val inflightCount = Output(UInt((indexWidth - reqSizeBit).W))
    val empty = Output(Bool())

    val ack = Input(Bool())
  })

  val first = RegInit(0.U(indexWidth.W))
  val last = RegInit(0.U(indexWidth.W))
  val req = RegInit(0.U(indexWidth.W))

//  val mem = Mem(entries, UInt(16.W))
  val mem = Reg(Vec(entries, UInt(16.W)))
  val noXcptLit = (new HeartXcpt).Lit(
    _.loc -> false.B,
    _.ma -> false.B,
    _.pf -> false.B,
    _.gf -> false.B,
    _.ae -> false.B,
  )
  val xcpt = RegInit(noXcptLit)
  val xcptPtr = RegInit(0.U(indexWidth.W))

  io.hasXcpt := xcpt.asUInt.orR
  // io.inflightCount := indexDiff(req, last) >> reqSizeBit.U
  io.empty := first === req

  when (io.flush) {
    last := 0.U
  }
  io.allocateSucceed := false.B

  // Max size is entries - 1
  val remainSize = (entries-1).U - (last - first)
  when (io.allocate) {
    when (io.flush) {
      last := io.allocSize +& 1.U
      io.allocateSucceed := true.B
    }.elsewhen(io.allocSize < remainSize) {
      last := last + io.allocSize +& 1.U
      io.allocateSucceed := true.B
    }
  }

  val inflightCount = RegInit(0.U(indexWidth.W))

  when (io.flush) {
    inflightCount := io.allocateSucceed.asUInt
  }.elsewhen(io.allocateSucceed ^ io.ack) {
    when(io.allocateSucceed) {
      inflightCount := inflightCount + 1.U
    }
    when(io.ack) {
      inflightCount := inflightCount - 1.U
    }
  }

  io.inflightCount := inflightCount

  when (io.enq) {
    xcpt := io.enqXcpt
    xcptPtr := req
    Seq.tabulate(reqSize) { i=>
      when (io.enqSize >= i.U) {
        mem(req + i.U) := io.enqData(16*(i+1) - 1, 16 * i)
      }
    }
    req := req + io.enqSize +& 1.U
  }
  when (io.flush) {
    req := 0.U
    xcpt := noXcptLit
    xcptPtr := 0.U
  }

  val ready = req - first
  when (ready >= 2.U) {
    io.canDeq := true.B
  }.elsewhen (ready === 1.U) {
    io.canDeq := mem(first)(1, 0) =/= "b11".U
  }.otherwise {
    io.canDeq := false.B
  }

  io.deqData := mem(first+1.U) ## mem(first)
  io.deqXcpt := Mux(first === xcptPtr || (first+1.U) === xcptPtr, xcpt, noXcptLit)
  when (io.deq) {
    first := first + Mux(io.deqData(1, 0) === "b11".U, 2.U, 1.U)
  }
  when (io.flush) {
    first := 0.U
  }
}


class Frontend(implicit p: Parameters) extends CoreModule {

  import FrontendControlIE._

  val k = p(KLASE32ParamKey)
  val NOP = BitPat("b00000000000000000000000000010011")

  val io = IO(new FrontendIO())

  val issueEntry = Wire(Decoupled(new IFIEPipelineEntry()))
  // After n cycle prefetchReq will start
  val regBooting = RegInit(true.B)

  val bootingCycle = 2 // booting cycle
  if (bootingCycle > 0) {
    val bootingCounter = RegInit(bootingCycle.U)
    bootingCounter := Mux(io.hartEn && regBooting, bootingCounter - 1.U, bootingCounter)
    regBooting := regBooting && bootingCounter =/= 0.U
  } else {
    when (io.hartEn && regBooting) {
      regBooting := false.B
    }
  }
  io.resetReq.booting := regBooting

  val bootAddr = if (usingOuterBootAddr) io.epm.bootAddr else bootAddrParam.U
  io.resetReq.bootAddr := bootAddr

  // Next PC
  val brCond = (io.ctrl === BR) & io.cnd
  val haltCond = (io.ctrl === HALT)
  val jalCond = (io.ctrl === JAL)
  val jalrCond = (io.ctrl === JALR)
  val jump = issueEntry.ready && (brCond || jalCond || jalrCond)
  io.jump := jump

  val sextBrOffset = Fill(xLen-13, io.brOffset(12)) ## io.brOffset
  val brAddr = io.ie_pc + sextBrOffset
  val jumpPC = Mux(brCond, brAddr, io.aluR)

  val dbgFire = io.debug.fire
  val dbgReturn = io.debug.mode && io.dret
  val dbgBreak = io.debug.mode && io.ebreak
  val dbgException = io.debug.mode && io.exception
  val exception = io.exception || io.eret
  val csrInst = io.flushFetchQueue.ie.csr.orR
  val wfi = io.wfi

  // To ease timing issue, those are not in normal program flow is delayed
  // When mispredict, flush
  // When it was correct prediction, do not flush
  // But both should jump current cycle
  val jumpNextCycle =
    dbgFire ||
      dbgReturn ||
      dbgBreak ||
      dbgException ||
      exception

  val regJumpNextCycle = RegNext(jumpNextCycle)
  val jumpCurrentCycle =
    jump ||
      csrInst ||
      wfi

  // Flush fetch queue
  val flush = io.flushFetchQueue.ie.jump ||
    io.flushFetchQueue.ie.csr ||
    io.flushFetchQueue.ie.wfi ||
    io.flushFetchQueue.ie.xcpt ||
      io.flushFetchQueue.ie.eret ||
      io.ebreak ||
      io.debug.fire

  val nextPrefetchAddr = RegInit(bootAddr)
  val targetAddr = Wire(UInt(k.vaddrBits.W))
  val nextTargetAddr = Reg(UInt(k.vaddrBits.W))

  val fq = Module(new FetchQueue())
  fq.io.flush := flush

  // All exceptions are handled in IE stage
  // If current control is 1 cycle fetch such as exception, do not request in 0 cycle
  fq.io.allocate := !regBooting && !io.wfiOut
  fq.io.allocSize := Mux(targetAddr(1,0) =/= 0.U, 0.U, 1.U)

  val prefetchReq = fq.io.allocateSucceed
  val prefetchAddr = targetAddr(31, 2) ## 0.U(2.W)
  when (prefetchReq) {
    nextPrefetchAddr := prefetchAddr + (k.fetchWidth * 4).U
  }

  when(dbgFire) {
    nextTargetAddr := debugAddrParam.U
  }.elsewhen(dbgReturn) {
    nextTargetAddr := io.debug.dpc
  }.elsewhen(dbgBreak) {
    nextTargetAddr := debugAddrParam.U
  }.elsewhen(dbgException) {
    nextTargetAddr := debugExceptionAddrParam.U
  }.elsewhen(exception) {
    nextTargetAddr := io.evec
  }.otherwise {
    nextTargetAddr := nextTargetAddr
  }

  when(regJumpNextCycle) {
    targetAddr := nextTargetAddr
  }.elsewhen(jump) {
    targetAddr := jumpPC
  }.elsewhen(io.flushFetchQueue.ie.csr.orR || io.wfi) {
    targetAddr := io.ie_pc + 4.U
  }.otherwise {
    targetAddr := nextPrefetchAddr
  }

  // 4 bytes align
  io.epm.addr := prefetchAddr
  // printf(cf"[FE] fetchPC: 0x${io.epm.addr}%x\n")
  io.epm.req := prefetchReq
  // FIXME: Should be handled in the future
  io.epm.flush := io.flushIcache.asUInt.orR
  io.epm.cmd := 0.U // int load
  io.epm.kill := 0.U

  val curPC = RegInit(bootAddr)

  fq.io.deq := issueEntry.ready && fq.io.canDeq
  val instRvc = fq.io.deqData(1, 0) =/= "b11".U
//  val rvcExpanded = new RVCDecoder(fq.io.deqData, xLen = xLen, useAddiForMv = false).decode.bits
  // To prevent unnecessary power consumption of rvc decoder
  val rvcExpanded = new RVCDecoder(Mux(instRvc, 0.U(16.W) ## fq.io.deqData(15,0), 0.U), xLen = xLen, useAddiForMv = false).decode.bits
  val instData = Mux(instRvc, rvcExpanded, fq.io.deqData)

  issueEntry.valid := fq.io.deq
  issueEntry.bits.pc := curPC

  issueEntry.bits.data := bitPatToUInt(NOP)
  issueEntry.bits.xcpt := fq.noXcptLit
  issueEntry.bits.rvc := false.B
  when (fq.io.canDeq) {
    issueEntry.bits.data := instData
    issueEntry.bits.xcpt := fq.io.deqXcpt
    issueEntry.bits.rvc := instRvc
  }

  io.issue <> issueEntry

  // Used to tval register update when illegal instruction exception occurs
  // And trigger, it should be used to raw instructions including RVC
  io.instRaw := (0.U(16.W) & fq.io.deqData(31,16)) ## fq.io.deqData(15,0)

  when(issueEntry.ready) {
    when(regJumpNextCycle || jumpCurrentCycle){
      curPC := targetAddr
    }.elsewhen(issueEntry.valid) {
      curPC := Mux(issueEntry.bits.rvc, curPC + 2.U, curPC + 4.U)
    }
  }.otherwise {
    curPC := curPC
  }


  val ignoreCount = RegInit(0.U(2.W))
  when (flush) {
    ignoreCount := fq.io.inflightCount - (io.epm.ack && io.epm.gnt) + (jumpNextCycle && fq.io.allocateSucceed)
  }.otherwise {
    ignoreCount := Mux(ignoreCount =/= 0.U, ignoreCount - 1.U, 0.U)
  }

  val ignore = flush || ignoreCount =/= 0.U

  // Only after jump can cause unaligned prefetchReq, And curPC has the target address of fetchedPacket address
  // After enqueued, curPC no more indicates the target address
  // Fetched data comes up in an incremental manner starting from requested address
  // Such as... requested address: 0x4, fetch width: 16Bytes -> Data address: 0x4, 0x8, ... , 0x20
  val unalignedFetchData = fq.io.empty && curPC(1,0) =/= 0.U
  fq.io.enqSize := Mux(unalignedFetchData, 0.U, 1.U)
  fq.io.enqData := Mux(unalignedFetchData, io.epm.data >> 16, io.epm.data)
  fq.io.enqXcpt := io.epm.xcpt
  fq.io.enq := io.epm.ack && !ignore && io.epm.gnt
  fq.io.ack := io.epm.ack && io.epm.gnt
}
