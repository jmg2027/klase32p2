package klase32.functionalunit

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import klase32.include.config._
import klase32.include.ControlSignal._
import klase32.include.KLASE32AbstractClass._
import klase32.include.param.KLASE32ParamKey
import klase32.common.FunctionUnitIO

class DIVReq(implicit p: Parameters) extends CoreBundle with HasCoreParameters {
  val ctrl = DIVControlIE()
  val A = SInt(mxLen.W)
  val B = SInt(mxLen.W)
  val rd = UInt(regIdWidth.W)
}

class DIVResp(implicit p: Parameters) extends CoreBundle with HasCoreParameters {
  val result = UInt(mxLen.W)
  val rd = UInt(regIdWidth.W)
}

class DIV(implicit p: Parameters) extends CoreModule
with FunctionUnitIO[DIVReq, DIVResp] {
  val req = IO(Flipped(Decoupled(new DIVReq)))
  val resp = IO(Decoupled(new DIVResp))

  val core = Module(new DIVCore)

  core.io.start := req.fire
  core.io.ctrl := req.bits.ctrl
  core.io.A := req.bits.A
  core.io.B := req.bits.B

  req.ready := core.io.ready
  resp.valid := core.io.valid
  resp.bits.result := core.io.out
  resp.bits.rd := req.bits.rd
}


class DIVCore(implicit p: Parameters) extends CoreModule {
  import DIVControlIE._
  val MINUS_ONE = (1 << xLen - 1).S.asUInt
  val k = p(KLASE32ParamKey)

    val io = IO(new Bundle{
      val start = Input(Bool())
      val ctrl = Input(DIVControlIE())
      val A = Input(SInt(xLen.W))
      val B = Input(SInt(xLen.W))
      val ready = Output(Bool())
      val valid = Output(Bool())
      val out = Output(UInt(xLen.W))
    }
  )

  val signed = io.ctrl === DIV || io.ctrl === REM
  val isRem = io.ctrl === REM || io.ctrl === REMU

  val divisor = Mux(signed && io.B < 0.S, -io.B, io.B).asUInt
  val dividend = Mux(signed && io.A < 0.S, -io.A, io.A).asUInt

  // State machine
  val sIdle :: sCompute :: sDone :: Nil = Enum(3)
  val state = RegInit(sIdle)
  val shiftRemainder = Reg(UInt((2*xLen+1).W))
  val counter = Reg(UInt(log2Ceil(xLen+1).W))
  val counterTarget = if (divParam.useClz) xLen.U - PriorityEncoder(Cat(divisor, 1.U)) else xLen.U

  io.ready := (state === sIdle)
  io.valid := (state === sDone)

  switch(state) {
    is(sIdle) {
      when(io.start) {
        // Special cases
        when(io.B === 0.S) {
          when(isRem) {
            io.out := io.A.asUInt
          }.otherwise {
            io.out := MINUS_ONE
          }
          state := sDone
        }.elsewhen(io.B === -1.S && io.A === (1 << (xLen - 1)).S) {
          when(io.ctrl === DIV) {
            io.out := io.A.asUInt
          }.elsewhen(io.ctrl === REM) {
            io.out := 0.U
          }
          state := sDone
        }.otherwise {
          shiftRemainder := Cat(0.U(xLen.W), dividend)
          counter := counterTarget
          state := sCompute
        }
      }
    }
    is(sCompute) {
      val subtractResult = shiftRemainder(2*xLen, xLen) - divisor
      val neg = subtractResult(xLen)
      shiftRemainder := Mux(neg, shiftRemainder(2*xLen-1, xLen), subtractResult(xLen-1, 0) ## shiftRemainder(xLen-1, 0) ## !neg)
      when(counter === 0.U) {
        state := sDone
      }.otherwise {
        counter := counter - 1.U
      }
    }
    is(sDone) {
      when(!io.start) {
        state := sIdle
      }
    }
  }

  val quotient = WireInit(0.U(xLen.W))
  val remainder = WireInit(0.U(xLen.W))
  when(signed && ((io.A < 0.S) =/= (io.B < 0.S))) {
    quotient := -shiftRemainder(xLen - 1, 0)
  }.otherwise {
    quotient := shiftRemainder(xLen - 1, 0)
  }
  when(signed && io.A < 0.S) {
    remainder := -shiftRemainder(2 * xLen, xLen + 1)
  }.otherwise {
    remainder := shiftRemainder(2 * xLen, xLen + 1)
  }
  io.out := Mux(isRem, remainder, quotient).asUInt
}
