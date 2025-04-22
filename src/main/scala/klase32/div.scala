package klase32

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import klase32.include.config._
import klase32.include.ControlSignal._
import klase32.include.KLASE32AbstractClass._
import klase32.include.param.KLASE32ParamKey


class DIV(implicit p: Parameters) extends CoreModule {
  import DIVControlIE._
  val MINUS_ONE = (1 << xLen - 1).S.asUInt
  val k = p(KLASE32ParamKey)

    val io = IO(new Bundle{
      val ctrl = Input(DIVControlIE())
      val A = Input(SInt(xLen.W))
      val B = Input(SInt(xLen.W))
      val res = Valid(Output(UInt(xLen.W)))

      val busy = Output(Bool())
    }
  )

  // State machine
  val sIdle :: sCompute :: sDone :: Nil = Enum(3)
  val state = RegInit(sIdle)

  val counter = RegInit(0.U(log2Ceil(xLen+1).W))

//  val shiftRemainder = RegInit(0.S((2*xLen+1).W))
  val shiftRemainder = Reg(UInt((2*xLen+1).W))


  // Assume control signal stalls until division is complete
  val start = io.ctrl =/= default
  val signed = io.ctrl === DIV || io.ctrl === REM
  val isRem = io.ctrl === REM || io.ctrl === REMU

  val divisor = Mux(signed && io.B < 0.S, -io.B, io.B).asUInt
  val dividend = Mux(signed && io.A < 0.S, -io.A, io.A).asUInt

  // Initialize IO
  io.res.valid := false.B
  io.res.bits := 0.U

  io.busy := false.B

  switch(state) {
    is(sIdle) {
      io.res.valid := false.B
      when(start) {
        io.busy := true.B
        // Special cases
        when(io.B === 0.S) {
          when(isRem) {
            io.res.valid := true.B
            io.res.bits := io.A.asUInt
          }.otherwise {
            io.res.valid := true.B
            io.res.bits := MINUS_ONE
          }
          state := sDone
        }.elsewhen(io.B === -1.S && io.A === (1 << (xLen - 1)).S) {
          when(io.ctrl === DIV) {
            io.res.valid := true.B
            io.res.bits := io.A.asUInt
          }.elsewhen(io.ctrl === REM) {
            io.res.valid := true.B
            io.res.bits := 0.U
          }
          state := sDone
//        }.elsewhen(signed) {
//          state := sNeg
        }.otherwise {
          shiftRemainder := Cat(0.U(xLen.W), dividend)
          state := sCompute
        }
      }
    }
//    is(sNeg) {
//      when(shiftRemainder(xLen-1)) {
//        shiftRemainder := -shiftRemainder
//      }
//      when(divisor(xLen-1)) {
//        divisor := shiftRemainder(2*xLen, xLen) - divisor
//      }
//      state := sCompute
//    }
    is(sCompute) {
      io.busy := true.B
      io.res.valid := false.B
      counter := counter + 1.U

      val subtractResult = shiftRemainder(2*xLen, xLen) - divisor

      val neg = subtractResult(xLen)
      shiftRemainder := Cat(Mux(neg, shiftRemainder(2*xLen-1, xLen), subtractResult(xLen-1, 0)), shiftRemainder(xLen-1, 0), !neg)
//
//      when(subtractResult((xLen-1)) === 0.U) {
//        shiftRemainder := ((subtractResult ## shiftRemainder(xLen-1, 0).asSInt).asSInt) | 1.S
////        shiftRemainder := (shiftRemainder << 1) | 1.S
////        shiftRemainder := shiftRemainder | 1.S
//      }.otherwise {
//        shiftRemainder := shiftRemainder << 1
//      }

      when(counter === (xLen).U) {
        state := sDone
      }
    }

    is(sDone) {
      counter := 0.U
      io.res.valid := true.B
      io.busy := false.B
      val quotient = WireInit(0.U(xLen.W))
      val remainder = WireInit(0.U(xLen.W))
//      when(!start) { // This will include stall signal by stalling decoder
      when(signed && ((io.A < 0.S) =/= (io.B < 0.S))) {
        quotient := -shiftRemainder(xLen-1, 0)
//          quotient := -quotient
//          shiftRemainder(xLen-1, 0) := -shiftRemainder(xLen-1, 0)
      }.otherwise {
        quotient := shiftRemainder(xLen-1, 0)
      }
      when(signed && io.A < 0.S) {
        remainder := -shiftRemainder(2*xLen, xLen+1)
//          shiftRemainder(2*xLen-1, xLen) := -shiftRemainder(2*xLen-1, xLen)
//          remainder := -remainder
      }.otherwise {
        remainder := shiftRemainder(2*xLen, xLen+1)
      }
      io.res.bits := Mux(isRem, remainder, quotient).asUInt
      state := sIdle
//      }
    }
  }
}
