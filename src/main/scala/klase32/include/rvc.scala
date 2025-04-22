package klase32.include

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import klase32.include.config._
import klase32.include.param.KLASE32ParamKey
import klase32.include.KLASE32AbstractClass._

import freechips.rocketchip.rocket.RVCDecoder

class RVCExpander(implicit p: Parameters) extends CoreModule {
  val io = IO(new Bundle {
    val in = Input(UInt(wordsize.W))
    val out = Output(UInt(wordsize.W))
  })

    if (usingCompressed) {
      when(io.in(1,0) =/= 3.U) {
        io.out := new RVCDecoder(io.in, xLen = xLen, useAddiForMv = false).decode.bits
      }.otherwise {
        io.out := io.in
      }
    } else {
      io.out := io.in
    }

}
