package klase32

import chisel3._
import chisel3.util._
import klase32.include.config._
import klase32.include.KLASE32AbstractClass._
import klase32.include.ControlSignal._


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
//  val regFile = Mem(regNum-1, UInt(mxLen.W))
  val regFile = Reg(Vec(regNum-1, UInt(mxLen.W)))
//  val regFile = Mem(regNum, UInt(mxLen.W))

//  for (i <- 1 until regNum) {
//    regFile(i).suggestName(s"x$i")
//  }
//  val x = WireInit(VecInit(Seq.tabulate(regNum){i => false.B}))
//    for (i <- 1 until regNum) {
//      x(i) := regFile(i-1)
//    }

  // This causes too much area overhead
//  val x1 = Wire(UInt(mxLen.W)) ; x1  := regFile(0 )
//  val x2 = Wire(UInt(mxLen.W)) ; x2  := regFile(1 )
//  val x3 = Wire(UInt(mxLen.W)) ; x3  := regFile(2 )
//  val x4 = Wire(UInt(mxLen.W)) ; x4  := regFile(3 )
//  val x5 = Wire(UInt(mxLen.W)) ; x5  := regFile(4 )
//  val x6 = Wire(UInt(mxLen.W)) ; x6  := regFile(5 )
//  val x7 = Wire(UInt(mxLen.W)) ; x7  := regFile(6 )
//  val x8 = Wire(UInt(mxLen.W)) ; x8  := regFile(7 )
//  val x9 = Wire(UInt(mxLen.W)) ; x9  := regFile(8 )
//  val x10 = Wire(UInt(mxLen.W)); x10 := regFile(9 )
//  val x11 = Wire(UInt(mxLen.W)); x11 := regFile(10)
//  val x12 = Wire(UInt(mxLen.W)); x12 := regFile(11)
//  val x13 = Wire(UInt(mxLen.W)); x13 := regFile(12)
//  val x14 = Wire(UInt(mxLen.W)); x14 := regFile(13)
//  val x15 = Wire(UInt(mxLen.W)); x15 := regFile(14)
//  val x16 = Wire(UInt(mxLen.W)); x16 := regFile(15)
//  val x17 = Wire(UInt(mxLen.W)); x17 := regFile(16)
//  val x18 = Wire(UInt(mxLen.W)); x18 := regFile(17)
//  val x19 = Wire(UInt(mxLen.W)); x19 := regFile(18)
//  val x20 = Wire(UInt(mxLen.W)); x20 := regFile(19)
//  val x21 = Wire(UInt(mxLen.W)); x21 := regFile(20)
//  val x22 = Wire(UInt(mxLen.W)); x22 := regFile(21)
//  val x23 = Wire(UInt(mxLen.W)); x23 := regFile(22)
//  val x24 = Wire(UInt(mxLen.W)); x24 := regFile(23)
//  val x25 = Wire(UInt(mxLen.W)); x25 := regFile(24)
//  val x26 = Wire(UInt(mxLen.W)); x26 := regFile(25)
//  val x27 = Wire(UInt(mxLen.W)); x27 := regFile(26)
//  val x28 = Wire(UInt(mxLen.W)); x28 := regFile(27)
//  val x29 = Wire(UInt(mxLen.W)); x29 := regFile(28)
//  val x30 = Wire(UInt(mxLen.W)); x30 := regFile(29)
//  val x31 = Wire(UInt(mxLen.W)); x31 := regFile(30)
//
//  val ra = Wire(UInt(mxLen.W)); ra := x1 ; dontTouch(ra )
//  val sp = Wire(UInt(mxLen.W)); sp := x2 ; dontTouch(sp )
//  val gp = Wire(UInt(mxLen.W)); gp := x3 ; dontTouch(gp )
//  val tp = Wire(UInt(mxLen.W)); tp := x4 ; dontTouch(tp )
//  val t0 = Wire(UInt(mxLen.W)); t0 := x5 ; dontTouch(t0 )
//  val t1 = Wire(UInt(mxLen.W)); t1 := x6 ; dontTouch(t1 )
//  val t2 = Wire(UInt(mxLen.W)); t2 := x7 ; dontTouch(t2 )
//  val s0 = Wire(UInt(mxLen.W)); s0 := x8 ; dontTouch(s0 )
//  val s1 = Wire(UInt(mxLen.W)); s1 := x9 ; dontTouch(s1 )
//  val a0 = Wire(UInt(mxLen.W)); a0 := x10; dontTouch(a0 )
//  val a1 = Wire(UInt(mxLen.W)); a1 := x11; dontTouch(a1 )
//  val a2 = Wire(UInt(mxLen.W)); a2 := x12; dontTouch(a2 )
//  val a3 = Wire(UInt(mxLen.W)); a3 := x13; dontTouch(a3 )
//  val a4 = Wire(UInt(mxLen.W)); a4 := x14; dontTouch(a4 )
//  val a5 = Wire(UInt(mxLen.W)); a5 := x15; dontTouch(a5 )
//  val a6 = Wire(UInt(mxLen.W)); a6 := x16; dontTouch(a6 )
//  val a7 = Wire(UInt(mxLen.W)); a7 := x17; dontTouch(a7 )
//  val s2 = Wire(UInt(mxLen.W)); s2 := x18; dontTouch(s2 )
//  val s3 = Wire(UInt(mxLen.W)); s3 := x19; dontTouch(s3 )
//  val s4 = Wire(UInt(mxLen.W)); s4 := x20; dontTouch(s4 )
//  val s5 = Wire(UInt(mxLen.W)); s5 := x21; dontTouch(s5 )
//  val s6 = Wire(UInt(mxLen.W)); s6 := x22; dontTouch(s6 )
//  val s7 = Wire(UInt(mxLen.W)); s7 := x23; dontTouch(s7 )
//  val s8 = Wire(UInt(mxLen.W)); s8 := x24; dontTouch(s8 )
//  val s9 = Wire(UInt(mxLen.W)); s9 := x25; dontTouch(s9 )
//  val s10= Wire(UInt(mxLen.W)); s10:= x26; dontTouch(s10)
//  val s11= Wire(UInt(mxLen.W)); s11:= x27; dontTouch(s11)
//  val t3 = Wire(UInt(mxLen.W)); t3 := x28; dontTouch(t3 )
//  val t4 = Wire(UInt(mxLen.W)); t4 := x29; dontTouch(t4 )
//  val t5 = Wire(UInt(mxLen.W)); t5 := x30; dontTouch(t5 )
//  val t6 = Wire(UInt(mxLen.W)); t6 := x31; dontTouch(t6 )

  for (i <- 0 until readportNum) {
    when(io.rp(i).addr === 0.U) {
      io.rp(i).data := 0.U
    } otherwise {
      io.rp(i).data := regFile(io.rp(i).addr-1.U)
    }
  }

  for (i <- 0 until writeportNum) {
    when(io.wp(i).valid) {
      regFile(io.wp(i).bits.addr-1.U) := io.wp(i).bits.data
    }
  }
}
