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

  val isRVC = Output(Bool())

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
  val instBufDataSliceIsRVC = WireDefault(0.U(numMask.W))
  val instBufDataSliceIndex = io.if_pc(log2Ceil(numMask) + 1, 1)
  val prevInstNotFinished = WireDefault(false.B)
  val instBufDataSlice = Wire(VecInit(Seq.fill(numMask)(0.U(16.W))))

  instBufDataSlice.zipWithIndex.foreach {
    case (slice, i) =>
      slice := instBuf.bits.data(16*(i+1)-1, 16*i)
      instBufDataSliceIsRVC(i) := slice(1,0) =/= 3.U
  }

  io.isRVC := instBufDataSliceIsRVC(instBufDataSliceIndex)

  // fetch new entry from fetch queue when core consumed all instructions or pc is out of range
  io.fetchQueueTail.ready := instBufEmpty

  // Enqueue
  when(io.fetchQueueTail.fire) {
    instBuf := io.fetchQueueTail.bits
    instBufEmpty := false.B
  }

  // Dequeue
  when(io.instPacket.fire) {
    io.instPacket.bits.xcpt := instBuf.bits.xcpt
    when(io.isRVC) {
      io.instPacket.bits.data := instBufDataSlice(instBufDataSliceIndex)
    }.elsewhen(instBufDataSliceIndex =/= (numMask.U - 1.U)) {
      io.instPacket.bits.data := instBufDataSlice(instBufDataSliceIndex + 1.U) ## instBufDataSlice(instBufDataSliceIndex)
    }.otherwise {
      
    }
  }

  // Check current instruction slice by PC

}
