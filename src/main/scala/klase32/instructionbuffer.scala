package klase32

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import klase32.config._
import klase32.param.KLASE32ParamKey

class InstructionBufferIO(implicit p: Parameters) extends CoreBundle with HasCoreParameters {
  val if_pc = Input(UInt(mxLen.W)) // Current PC

  val stall = Input(Bool())
  val wfi = Input(Bool())

  val issue = Output(Bool())

  val fetchQueueTail = Flipped(Decoupled(new FetchQueueEntry()))
  val instPacket = Decoupled(new InstructionPacket())

}

class InstructionBuffer(implicit p: Parameters) extends CoreModule {
  val k = p(KLASE32ParamKey)
  val numMask = log2Ceil(k.fetchWidth/8)

  val io = IO(new InstructionBufferIO)

  val instBuf = RegInit(0.U.asTypeOf(io.fetchQueueTail))
  val instBufEmpty = WireDefault(true.B)
  val bufMaskIsRVC = WireDefault(0.U(numMask.W))
  val bufMaskIndex = io.if_pc(log2Ceil(numMask) + 1, 1)
  val prevMSB16IsRVI = WireDefault(false.B)
  val instBufDataSlice = VecInit(instBuf.bits.data.

  // fetch new entry from fetch queue when core consumed all instructions or pc is out of range
  io.fetchQueueTail.ready := instBufEmpty

  when(io.fetchQueueTail.fire) {
    instBuf := io.fetchQueueTail.bits
    instBufEmpty := false.B
  }

}
