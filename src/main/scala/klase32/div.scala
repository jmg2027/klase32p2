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

    // Use dividend register as quotient/remainder
    val regAddr = Input(UInt(regIdWidth.W))

    }
  )

  val valid = io.ctrl =/= default
  val signed = io.ctrl === DIV && io.ctrl === REM
  val isRem = io.ctrl === REM && io.ctrl === REMU

}
