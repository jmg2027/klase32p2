package klase32

import chisel3._
import chisel3.util._
import klase32.config._

class ReadportIntfIO(implicit p: Parameters) extends CoreBundle with HasCoreParameters {
  val data = Output(UInt(mxLen.W))
  val addr = Input(UInt(regIdWidth.W))

}
class WriteportIntf(implicit p: Parameters) extends CoreBundle with HasCoreParameters {
  val data = UInt(mxLen.W)
  val addr = UInt(regIdWidth.W)
}


class RegisterFileIO(implicit p: Parameters) extends CoreBundle with HasCoreParameters {
  val rp = Vec(readportNum, new ReadportIntfIO())
  val wp = Flipped(Vec(writeportNum, Valid(new WriteportIntf())))
}

class RegisterFile(implicit p: Parameters) extends CoreModule with HasCoreParameters {
  val io = IO(new RegisterFileIO())
  val regFile = Mem(regNum, UInt(mxLen.W))
  // x0 is zero register
  regFile(0) := 0.U

  for (i <- 0 until readportNum) {
    io.rp(i).data := regFile(io.rp(i).addr)
  }

  for (i <- 0 until writeportNum) {
    when (io.wp(i).valid) {
      regFile(io.wp(i).bits.addr) := io.wp(i).bits.data
    }
  }
}
