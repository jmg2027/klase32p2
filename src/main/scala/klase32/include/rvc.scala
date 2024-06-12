package klase32

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import klase32.config._
import klase32.param.KLASE32ParamKey


class RVCIO(implicit p: Parameters) extends CoreBundle {
  val in = Flipped(Decoupled(new InstructionPacket()))
  val pcIs4ByteAligned = Input(Bool())
  val out = Decoupled(new InstructionPacket())

  val pcStepByFour = Output(Bool())
}


class RVC(implicit p: Parameters) extends CoreModule {
  val io = IO(new RVCIO())

  val rvcExpander = Module(new RVCExpander())

  val instHalfBuf = RegInit(Valid(0.U(16.W)))
  val itTakesTwoCycles = RegInit(false.B)

  val inHalfHigh = io.in.bits.data(mxLen-1, mxLen/2)
  val inHalfLow = io.in.bits.data(mxLen/2, 0)

  // 32
  // 16/16
  // 32(0)/16
  // 16/32(1)
  // 32(0)/32(1)
  val inIsRVI = !instHalfBuf.valid && !isRVC(inHalfLow) && io.pcIs4ByteAligned
  val inIsRVCAndRVC = !instHalfBuf.valid && isRVC(inHalfHigh) && isRVC(inHalfLow)
  val inIsRVIAndRVC = !instHalfBuf.valid && isRVC(inHalfHigh) && !isRVC(inHalfLow)
  val inIsRVCAndRVI = instHalfBuf.valid && isRVC(inHalfHigh)
  val inIsRVIAndRVI = instHalfBuf.valid && !isRVC(inHalfHigh)
//  val itTakesTwoCycles = inIsRVCAndRVC || inIsRVCAndRVI

  when(itTakesTwoCycles) {
    io.out.valid := true.B
    io.out.bits := instHalfBuf.bits
    instHalfBuf.valid := false.B
    itTakesTwoCycles := false.B
    io.in.ready := true.B
  }

  // Step by step case
  // when(io.out.ready)
  when(inIsRVI) {
    instHalfBuf.valid := false.B
    io.out <> io.in
    io.in.ready := true.B
    io.pcStepByFour := true.B
  }.elsewhen(inIsRVCAndRVC) {
    instHalfBuf.valid := false.B
    io.out.valid := true.B
    // PC value keeps feeding
    when(io.pcIs4ByteAligned) {
      expandRVCInst(inHalfLow)
      io.in.ready := false.B
      instHalfBuf.bits := inHalfHigh
      io.pcStepByFour := false.B
      itTakesTwoCycles := true.B
    }.otherwise {
      expandRVCInst(inHalfHigh)
      io.out.valid := true.B
      io.in.ready := true.B
      io.pcStepByFour := false.B
    }
  }.elsewhen(inIsRVIAndRVC) {
    instHalfBuf.valid := true.B
    instHalfBuf.bits := inHalfHigh
    when(io.pcIs4ByteAligned) {
      expandRVCInst(inHalfLow)
      io.out.valid := true.B
      io.in.ready := false.B
      io.pcStepByFour := false.B
    }.otherwise {
      io.out.valid := false.B
      io.in.ready := false.B
      io.pcStepByFour := true.B
    }
  }.elsewhen(inIsRVCAndRVI) {
    instHalfBuf.valid := false.B
    when(io.pcIs4ByteAligned) {
      instHalfBuf.bits := inHalfHigh
      io.out.bits.data := expandRVIInst
      io.out.valid := true.B
      io.pcStepByFour := true.B
      itTakesTwoCycles := true.B
    }.otherwise {
      io.out.bits := inHalfHigh
      io.out.valid := true.B
      io.pcStepByFour := false.B
    }
  }.elsewhen(inIsRVIAndRVI) {
    instHalfBuf.valid := true.B
    instHalfBuf.bits := inHalfHigh
    io.out.valid := true.B
    io.out.bits.data := expandRVIInst
    io.pcStepByFour := true.B
  }


  // It means rvc module has instruction to be issued
//  when(!io.in.ready)

  def isRVC(inst: UInt): Bool = inst(0,1) =/= 3.U
  def expandRVCInst(inst: UInt): Unit = {
    val rvc = Wire(inst.cloneType)
    rvcExpander.io.in := inst
    rvc := rvcExpander.io.out
    rvc
  }
  def expandRVIInst: UInt = Cat(inHalfLow, instHalfBuf.bits)

}

class RVCExpander(implicit p: Parameters) extends CoreModule {
  val io = IO(new Bundle{
    val in = UInt((mxLen/2).W)
    val out = UInt(mxLen.W)
  })
}
