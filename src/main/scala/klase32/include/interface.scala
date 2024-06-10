package klase32

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.util._
import klase32.config._
import klase32.param._
import snitch.enums._
import freechips.rocketchip.util._

object Acc {
 class Request(implicit p: Parameters) extends Bundle {
  val k = p(KLASE32ParamKey)

  val addr = UInt(k.addrWidth.W)
  val id = UInt(k.accidWidth.W)
  val op = UInt(32.W)
  val argA = UInt(k.accdataWidth.W)
  val argB = UInt(k.accdataWidth.W)
  val argC = UInt(k.accdataWidth.W)
 }

 class Response(implicit p: Parameters) extends Bundle {
  val k = p(KLASE32ParamKey)

  val id = UInt(k.accidWidth.W)
  val data = UInt(k.accdataWidth.W)
  val error = Bool()
 }

 class Interface(implicit p: Parameters) extends Bundle {
  val req = Decoupled(new Request)
  val resp = Flipped(Decoupled(new Response))
 }
}

class Interrupt extends Bundle {
 val e = Bool() // external
 val t = Bool() // timer
 val s = Bool() // software

 def &(op: Interrupt) = {
  val ret = Wire(new Interrupt())
  val eltret = (this.getElements zip op.getElements).map {
   case (a, b) => a.asInstanceOf[UInt] & b.asInstanceOf[UInt]
  }
  (ret.getElements zip eltret).foreach {
   case (a, b) => a := b
  }
  ret
 }

 def orR = this.getElements.map(_.asInstanceOf[Bool]).reduce(_ || _)
}

object Interrupt {
 def default = {
  (new Interrupt).Lit(
   _.e -> false.B,
   _.t -> false.B,
   _.s -> false.B,
  )
 }
}

class StallBundle extends Bundle {
 def orR = this.asUInt.orR
}

class Stall extends StallBundle {
 val ie = new StallBundle {
  val issue = Bool()
  val store = Bool()
  val csr = Bool()
 }
 val me = new StallBundle {
  val load = Bool()
  val hzd = Bool()
  val fence = Bool()
  val wfi = Bool()
 }
}

class FlushBundle extends Bundle {
 def orR = this.asUInt.orR
}

class Flush extends FlushBundle {
 val ie = new FlushBundle {
  val jump = Bool()
  val csr = Bool()
  val fence = Bool()
  val xcpt = Bool()
  val eret = Bool()
 }
// val me = new FlushBundle {
//  val load = Bool()
//  val hzd = Bool()
//  val fence = Bool()
//  val wfi = Bool()
// }
}

class ExternalMemoryInterfaceReq(implicit p: Parameters) extends Bundle {
 val k = p(KLASE32ParamKey)

 val addr = UInt(k.dataWidth.W)
 val numByte = UInt(2.W)
}

class ExternalMemoryInterfaceResp(implicit p: Parameters) extends Bundle {
 val k = p(KLASE32ParamKey)
 val rdata = UInt(k.dataWidth.W)
}

class HeartXcpt extends Bundle {
 val loc = Bool() // location of misaligned
 val ma = Bool() // misaligned exception
 val pf = Bool() // page fault exception
 val gf = Bool() // guest page fault exception
 val ae = Bool() // access exception
}

class EpmIntf(implicit p: Parameters) extends CoreBundle {
 val k = p(KLASE32ParamKey)

 val cmd = Output(UInt(5.W))
 val req = Output(Bool())
 val addr = Output(UInt(k.addrWidth.W))
 val gnt = Input(Bool())

 val ack = Input(Bool())
 val bootAddr = Input(UInt(k.addrWidth.W))

 val kill = Output(Bool())
 val flush = Output(Bool())

 val data = Input(UInt(k.fetchWidth.W))
 val xcpt = Input(new HeartXcpt)
}

class FetchQueueEntry(implicit p: Parameters) extends CoreBundle {
 val k = p(KLASE32ParamKey)

 val data = UInt(k.fetchWidth.W)
 val xcpt = new HeartXcpt
}


class StoreBufferEntry(implicit p: Parameters) extends CoreBundle {
 val k = p(KLASE32ParamKey)

// val addr = UInt(k.addrWidth.W)
// val data = UInt(xLen.W)
// val mask = UInt((wordsize/8).W)
 val valid = Bool()
}

class EdmIntf(implicit p: Parameters) extends CoreBundle {
 val k = p(KLASE32ParamKey)
 val maskBits = k.dataWidth / 8

 val cmd = Output(UInt(5.W)) // with dm_st_req or dm_ld_req
 val is_mmio = Input(Bool()) // with dm_st_ack
 val st_mmio_reserv = Output(Bool()) // with dm_ld_req
 val xcpt = Input(new HeartXcpt) // with dm_ld_ack

 val ld_req = Output(Bool()) // load or check write permission
 val ld_vaddr = Output(UInt(k.addrWidth.W)) // REMARK::THIS IS VADDR
 val ld_gnt = Input(Bool())

 val ld_ack = Input(Bool())
 val ld_rdata = Input(UInt(k.dataWidth.W))
 val ld_ppn = Input(UInt(22.W)) // only valid when dm_ld_req && dm_cmd.isOneOf(M_XWR, M_PWR)
 val ld_replay = Input(Bool()) // ld_replay is asserted when ld_req is for mmio and st_mmio_reserved is 1
 val ld_kill = Output(Bool())
 val ld_mmio_kill = Output(Bool())

 val st_req = Output(Bool()) // committed store request
 val st_paddr = Output(UInt(k.addrWidth.W)) // REMARK::THIS IS PADDR
 val st_wdata = Output(UInt(k.dataWidth.W)) // this can be used for AMO with dm_ld_req
 val st_mask = Output(UInt(maskBits.W))
 val st_mmio = Output(Bool()) // with dm_ld_req
 val st_gnt = Input(Bool())
 val st_ack = Input(Bool())

 val resp_cmd = Input(UInt(5.W)) // for debugging using vcore
 val resp_vaddr = Input(UInt(k.addrWidth.W))
 val resp_paddr = Input(UInt(k.addrWidth.W))
}


class JtagIntf extends Bundle {
 // JMG: Check
 //val tck = Input(Bool())
 val trst = Input(AsyncReset())
 val tck = Input(Clock())
 //val trst = Input(Reset())
 val ireg = Input(UInt(16.W))
 val si = Input(Bool())
 val so = Output(Bool())
 val capture_dr = Input(Bool())
 val update_dr = Input(Bool())
 val shift_dr = Input(Bool())
 val update_ir = Input(Bool())
 /*

   } // leaving gated-clock domain
   val rocketImpl = withClock (gated_clock) { new RocketImpl }
   DebugTransport.scala

  */
}

class DbgIntf extends Bundle {
 val extBreak = Input(Bool())
 val setBreak = Input(Bool())
}

class LSUControl extends Bundle {
 val lsSize = (DataSize())
 val isStore = (StoreControl())
 val isLoad = (LoadControl())
 val isSigned = (SignedControl())
}

case object NOP
