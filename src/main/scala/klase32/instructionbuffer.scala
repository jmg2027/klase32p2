package klase32

import chisel3._
import chisel3.util._
import chisel3.util.BitPat.bitPatToUInt
import chisel3.experimental.BundleLiterals._
import klase32.config._
import klase32.param.KLASE32ParamKey

class InstructionBufferIO(implicit p: Parameters) extends CoreBundle with HasCoreParameters {
  val if_pc = Input(UInt(mxLen.W)) // Current PC

  val curInstIsRVC = Output(Bool())

  val fetchQueueTail = Flipped(Decoupled(new FetchQueueEntry()))
  val instPacket = Decoupled(new InstructionPacket())

  val stall = Input(Bool())
}

// Use flush signal to reset
class InstructionBuffer(implicit p: Parameters) extends CoreModule {
  val NOP = BitPat("b00000000000000000000000000010011")
  val k = p(KLASE32ParamKey)
  val numMask = k.fetchBits/16

  val io = IO(new InstructionBufferIO)

  val rvc = Module(new RVCExpander())

  val instBuf = RegInit(0.U.asTypeOf(io.fetchQueueTail.bits))
  val instBufLastEntry = RegInit(false.B)
  val instBufValid = RegInit(false.B)
//  val instBufDataSliceIsRVC = WireDefault(0.U(numMask.W))
  val instBufDataSliceIsRVC = VecInit(Seq.fill(numMask)(false.B))
  val instBufDataSliceIndex = io.if_pc(log2Ceil(numMask), 1)
  val prevInstNotFinished = RegInit(false.B)
  val instBufDataSlice = VecInit(Seq.fill(numMask)(0.U(16.W)))
  val prevInstBuf = RegInit(0.U(16.W))

  instBufDataSlice.zipWithIndex.foreach {
    case (slice, i) =>
      when(instBufValid) {
        slice := instBuf.data(16 * (i + 1) - 1, 16 * i)
        instBufDataSliceIsRVC(i) := (slice(1, 0) =/= 3.U)
      }
  }

  io.curInstIsRVC := instBufDataSliceIsRVC(instBufDataSliceIndex)

  // fetch new entry from fetch queue when core consumed all instructions or pc is out of range
  instBufLastEntry := ((instBufDataSliceIndex === (numMask - 2).U && !io.curInstIsRVC) ||
    (instBufDataSliceIndex === (numMask - 1).U && io.curInstIsRVC)) && instBufValid

  // FIXME: What should be the condition??
  io.fetchQueueTail.ready := (!instBufValid || (instBufValid && instBufLastEntry)) && !io.stall
  io.instPacket.valid := instBufLastEntry && !prevInstNotFinished && instBufValid

  prevInstNotFinished := !io.curInstIsRVC && (instBufDataSliceIndex === (numMask.U - 1.U))
  when(prevInstNotFinished) {
    prevInstBuf := instBufDataSlice((numMask-1).U)
  }

  // Enqueue
  when(io.fetchQueueTail.fire) {
    instBufValid := true.B
    instBuf := io.fetchQueueTail.bits
  }

  io.instPacket.bits.xcpt := 0.U.asTypeOf(new HeartXcpt)
  io.instPacket.bits.data := bitPatToUInt(NOP)

  rvc.io.in := bitPatToUInt(NOP)
  // Dequeue
  when(io.instPacket.fire) {
    io.instPacket.bits.xcpt := instBuf.xcpt
    io.instPacket.bits.data := rvc.io.out
    when(io.curInstIsRVC) {
      // Can fix this to accept 16bits
      rvc.io.in := 0.U(16.W) ## instBufDataSlice(instBufDataSliceIndex)
    }.elsewhen(instBufDataSliceIndex =/= (numMask.U - 1.U)) {
      rvc.io.in:= instBufDataSlice(instBufDataSliceIndex + 1.U) ## instBufDataSlice(instBufDataSliceIndex)
    }.otherwise {
      rvc.io.in := instBufDataSlice(0) ## prevInstBuf
    }
  }
}
