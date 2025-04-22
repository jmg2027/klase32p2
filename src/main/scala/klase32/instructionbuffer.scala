package klase32

import chisel3._
import chisel3.util._
import chisel3.util.BitPat.bitPatToUInt
import chisel3.experimental.BundleLiterals._
import klase32.include.config._
import klase32.include.param.KLASE32ParamKey
import klase32.include.KLASE32AbstractClass._
import klase32.include.{FetchQueueEntry, HeartXcpt, InstructionPacket, RVCExpander}

class InstructionBufferIO(implicit p: Parameters) extends CoreBundle with HasCoreParameters {
  val if_pc = Input(UInt(mxLen.W)) // Current PC

  val curInstIsRVC = Output(Bool())

  val fetchQueueTail = Flipped(Decoupled(new FetchQueueEntry()))
  val instPacket = Decoupled(new InstructionPacket())

  val stall = Input(Bool())
  val flush = Input(Bool())

  val if_inst_raw = Output(UInt(xLen.W)) // IE stage instruction before RVC decoding
}

// Use flush signal to reset
class InstructionBuffer(implicit p: Parameters) extends CoreModule {
  val NOP = BitPat("b00000000000000000000000000010011")
  val k = p(KLASE32ParamKey)
  val numMask = k.fetchBits/16

  val io = IO(new InstructionBufferIO)

  val rvc = Module(new RVCExpander())

  val instBuf = RegInit(0.U.asTypeOf(io.fetchQueueTail.bits))
  val instBufValid = RegInit(false.B)
  val instBufDataSliceIsRVC = VecInit(Seq.fill(numMask)(false.B))
  val instBufDataSliceIndex = io.if_pc(log2Ceil(numMask), 1)
  val instBufDataSlice = VecInit(Seq.fill(numMask)(0.U(16.W)))
  val prevInstBufHi32 = RegInit(0.U(16.W))
  val instMisaligned = RegInit(false.B)

  // Handling last 32 bits of instruction buffer
/*

instMisaligned: next pc points prevbuf(-2) and not rvc

pc	misalign	rvc(N-2)	rvc(N-1)		fetch	instbufvalid	misalign	inst
0  false	true	true		false	true		false	instbuf lo 16

0  false	true	false		true	true		true	instbuf lo 1
0  false	false	dontcare		true	true		false	instbuf hi16 ## instbuf lo16

2	false	dontcare	true		true	true		false	instbuf hi 16
2  false	dontcare	false		true	false		true	x

2  true	dontcare	true		false	true		false	instbuf lo 16 ## prevbuf
2  true	dontcare	false		true	true		true	instbuf lo 16 ## prevbuf6
 */


  val fetchQueueReady = Wire(Bool())
  val instBufDataSliceMinus2isRVC = instBufDataSliceIsRVC(numMask-2)
  val instBufDataSliceMinus1isRVC = instBufDataSliceIsRVC(numMask-1)
  val instBufDataSliceIndexIsMinus2 = instBufDataSliceIndex === (numMask - 2).U
  val instBufDataSliceIndexIsMinus1 = instBufDataSliceIndex === (numMask - 1).U
  io.fetchQueueTail.ready := fetchQueueReady && !io.stall

  when(!instBufValid) {
    instMisaligned := false.B
  }.elsewhen(!io.stall) {
  when(instBufDataSliceIndexIsMinus2 && instBufDataSliceMinus2isRVC && !instBufDataSliceMinus1isRVC) {
    instMisaligned := true.B
  }.elsewhen(instBufDataSliceIndexIsMinus1 && !instMisaligned && instBufDataSliceMinus1isRVC) {
    instMisaligned := false.B
  }.elsewhen(instBufDataSliceIndexIsMinus1 && !instMisaligned && !instBufDataSliceMinus1isRVC) {
    instMisaligned := true.B
  }.elsewhen(instBufDataSliceIndexIsMinus1 && instMisaligned && instBufDataSliceMinus1isRVC) {
    instMisaligned := false.B
  }.elsewhen(instBufDataSliceIndexIsMinus1 && instMisaligned && !instBufDataSliceMinus1isRVC) {
    instMisaligned := true.B
  }

  }
  when(!instBufValid) {
    fetchQueueReady := true.B
    io.instPacket.valid := false.B
  }.otherwise {
    // Points other entries
    when(!(instBufDataSliceIndex === (numMask - 2).U) && !(instBufDataSliceIndex === (numMask - 1).U)) {
      fetchQueueReady := false.B
      io.instPacket.valid := true.B
      // Points numMask - 2
    }.elsewhen(instBufDataSliceIndexIsMinus2 && instBufDataSliceMinus2isRVC && instBufDataSliceMinus1isRVC) {
      fetchQueueReady := false.B
      io.instPacket.valid := true.B
    }.elsewhen(instBufDataSliceIndexIsMinus2 && instBufDataSliceMinus2isRVC && !instBufDataSliceMinus1isRVC) {
      fetchQueueReady := true.B
      io.instPacket.valid := true.B
//      instMisaligned := true.B
    }.elsewhen(instBufDataSliceIndexIsMinus2 && !instBufDataSliceMinus2isRVC) {
      fetchQueueReady := true.B
      io.instPacket.valid := true.B
      // Points numMask-1
    }.elsewhen(instBufDataSliceIndexIsMinus1 && !instMisaligned && instBufDataSliceMinus1isRVC) {
      fetchQueueReady := true.B
      io.instPacket.valid := true.B
//      instMisaligned := false.B
    }.elsewhen(instBufDataSliceIndexIsMinus1 && !instMisaligned && !instBufDataSliceMinus1isRVC) {
      fetchQueueReady := true.B
      io.instPacket.valid := false.B
//      instMisaligned := true.B
      // Points prevBuf
    }.elsewhen(instBufDataSliceIndexIsMinus1 && instMisaligned && instBufDataSliceMinus1isRVC) {
      fetchQueueReady := false.B
      io.instPacket.valid := true.B
//      instMisaligned := false.B
    }.elsewhen(instBufDataSliceIndexIsMinus1 && instMisaligned && !instBufDataSliceMinus1isRVC) {
      fetchQueueReady := true.B
      io.instPacket.valid := true.B
//      instMisaligned := true.B
    }.otherwise {
      fetchQueueReady := false.B
      io.instPacket.valid := true.B
    }
  }

  instBufDataSlice.zipWithIndex.foreach {
    case (slice, i) =>
//      when(instBufValid) {
        slice := instBuf.data(16 * (i + 1) - 1, 16 * i)
        instBufDataSliceIsRVC(i) := (slice(1, 0) =/= 3.U)
//      }
  }

  when(instMisaligned && instBufDataSliceIndexIsMinus1) {
    io.curInstIsRVC := false.B
  }.otherwise {
    io.curInstIsRVC := instBufDataSliceIsRVC(instBufDataSliceIndex)
  }

  // Enqueue
//  instBufValid := io.fetchQueueTail.fire
    when(!instBufValid) {
    instBufValid := io.fetchQueueTail.fire
  }.elsewhen(instBufValid && io.fetchQueueTail.fire) {
    instBufValid := true.B
  }.elsewhen(instBufValid && io.instPacket.fire && io.fetchQueueTail.ready) {
    instBufValid := false.B
  }
//  instBufValid := io.fetchQueueTail.fire
  when(io.fetchQueueTail.fire) {
//    instBufValid := true.B
    instBuf := io.fetchQueueTail.bits
    prevInstBufHi32 := instBufDataSlice((numMask-1).U)
//  }.otherwise {
//    instBufValid := false.B
  }
//
//  io.instPacket.bits.xcpt := 0.U.asTypeOf(new HeartXcpt)
//  io.instPacket.bits.data := bitPatToUInt(NOP)

  rvc.io.in := bitPatToUInt(NOP)
  io.if_inst_raw := rvc.io.in
  // Dequeue
  when(io.instPacket.fire) {
    io.instPacket.bits.xcpt := instBuf.xcpt
    io.instPacket.bits.data := rvc.io.out
    when(io.curInstIsRVC) {
      rvc.io.in := 0.U(16.W) ## instBufDataSlice(instBufDataSliceIndex)
    }
    .elsewhen(instBufDataSliceIndex =/= (numMask.U - 1.U)) {
      rvc.io.in:= instBufDataSlice(instBufDataSliceIndex + 1.U) ## instBufDataSlice(instBufDataSliceIndex)
    }.otherwise {
      rvc.io.in := instBufDataSlice(0) ## prevInstBufHi32
    }
  }.otherwise {
    io.instPacket.bits.xcpt := 0.U.asTypeOf(new HeartXcpt)
    io.instPacket.bits.data := bitPatToUInt(NOP)
  }
}
