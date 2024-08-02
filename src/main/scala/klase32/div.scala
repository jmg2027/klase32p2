package klase32

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import klase32.config._
import klase32.param.KLASE32ParamKey
// import klase32.interface._


class DIV(implicit p: Parameters) extends CoreModule {
  import DIVControlIE._
  val k = p(KLASE32ParamKey)

  val io = IO(new Bundle{
    val ctrl = Input(DIVControlIE())
    val A = Input(SInt(xLen.W))
    val B = Input(SInt(xLen.W))
    val res = Output(SInt(xLen.W))

    // Subtract operations are performed in the ALU
    val aluCtrl = Output(ALUControlIE())
    val aluA = Output(UInt(xLen.W))
    val aluB = Output(UInt(xLen.W))
    val aluR = Input(UInt(xLen.W))

    val busy = Output(Bool())
    }
  )

  // State machine
  val sIdle :: sCompute :: sDone :: Nil = Enum(3)
  val state = RegInit(sIdle)

  val quotient = RegInit(0.S(xLen.W))
  val remainder = RegInit(0.S(xLen.W))

  // Assume control signal stalls until division is complete
  val valid = io.ctrl =/= default
  val signed = io.ctrl === DIV && io.ctrl === REM
  val isRem = io.ctrl === REM && io.ctrl === REMU

  val divisor = Mux(signed && io.B < 0.S, -io.B, io.B)
  val dividend = Mux(signed && io.A < 0.S, -io.A, io.A)

  switch(state) {
    is(sIdle) {
      when(valid) {
        quotient := 0.S
        remainder := dividend
        state := sCompute
      }
    }

    is(sCompute) {
      val shiftRemainder = (remainder << 1).asSInt | (dividend((xLen)-1)).asSInt
      io.aluCtrl := ALUControlIE.SUB
      io.aluA := shiftRemainder.asUInt
      io.aluB := divisor.asUInt

      val subtractResult = io.aluR

      when(subtractResult((xLen-1)) === 0.U) {
        remainder := subtractResult
        quotient := (quotient << 1).asSInt | 1.S
      }.otherwise {
        remainder := shiftRemainder
        quotient := (quotient << 1).asSInt
      }

      dividend := dividend << 1

      when(dividend === 0.S) {
        state := sDone
      }
    }

    is(sDone) {
      when(!valid) {
        when(signed && ((io.A < 0.S) =/= (io.B < 0.S))) {
          quotient := -quotient
        }
        when(signed && io.A < 0.S) {
          remainder := -remainder
        }
        state := sIdle
      }
    }
  }

  io.res := Mux(isRem, remainder, quotient)
  io.busy := state =/= sIdle
}

