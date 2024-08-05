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
      val res = Valid(Output(UInt(xLen.W)))

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
  val start = io.ctrl =/= default
  val signed = io.ctrl === DIV && io.ctrl === REM
  val isRem = io.ctrl === REM && io.ctrl === REMU

  val divisor = Mux(signed && io.B < 0.S, -io.B, io.B)
  val dividend = Mux(signed && io.A < 0.S, -io.A, io.A)

  // Initialize IO
  io.aluCtrl := ALUControlIE.default
  io.aluA := 0.U
  io.aluB := 0.U

  io.res.valid := false.B
  io.res.bits := Mux(isRem, remainder, quotient).asUInt
  // Special cases
  when(io.B === 0.S) {
    when(isRem) {
      io.res.valid := true.B
      io.res.bits := io.A.asUInt
    }.otherwise {
      io.res.valid := true.B
      io.res.bits := (-1.S).asUInt
    }
  }.elsewhen(io.B === -1.S && io.A === (1 << (xLen-1)).S) {
    when(io.ctrl === DIV) {
      io.res.valid := true.B
      io.res.bits := io.A.asUInt
    }.elsewhen(io.ctrl === REM) {
      io.res.valid := true.B
      io.res.bits := 0.U
    }
  }.otherwise { // normal operation
    switch(state) {
      is(sIdle) {
        io.res.valid := false.B
        when(start) {
          quotient := 0.S
          remainder := dividend
          state := sCompute
        }
      }
      is(sCompute) {
        io.res.valid := false.B
        val shiftRemainder = (remainder << 1).asSInt | (dividend((xLen)-1)).asSInt
        io.aluCtrl := ALUControlIE.SUB
        io.aluA := shiftRemainder.asUInt
        io.aluB := divisor.asUInt

        val subtractResult = io.aluR.asSInt

        when(subtractResult((xLen-1)) === 0.U) {
          remainder := subtractResult
          quotient := (quotient << 1).asSInt | 1.S
        }.otherwise {
          remainder := shiftRemainder
          quotient := (quotient << 1).asSInt
        }
        dividend << 1

        when(dividend === 0.S) {
          state := sDone
        }
      }

      is(sDone) {
        io.res.valid := true.B
        when(!start) { // This will include stall signal by stalling decoder
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
  }
  io.busy := state =/= sIdle
}
