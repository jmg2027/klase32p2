package klase32

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import klase32.config._
import klase32.param.KLASE32ParamKey

import freechips.rocketchip.rocket.{RVCDecoder, ExpandedInstruction}

class RVCExpander(implicit p: Parameters) extends CoreModule {
  val io = IO(new Bundle {
    val in = UInt(wordsize.W)
    val out = UInt(wordsize.W)
  })

    if (usingCompressed) {
      io.out := new RVCDecoder(io.in, xLen = xLen, useAddiForMv = true).decode.bits
    } else {
      io.out := io.in
    }

}
