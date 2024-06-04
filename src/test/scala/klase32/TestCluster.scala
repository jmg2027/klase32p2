//package snitch.cluster
package klase32.cluster
//package klase32

import scala.collection.{mutable => mut}
import scala.Enumeration
import chisel3._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import klase32.{EdmIntf, EpmIntf}

//import snitch.{interface => inf}
import snitch.types._
import snitch.enums._
import klase32.config._

import assembler.RISCVAssembler
//import snitch.param.SnitchParamKey
import klase32.param.KLASE32ParamKey
import klase32.HeartXcpt

import snitch.test.TestMem

case class TestInterrupt(var e: Boolean, var t: Boolean, var s: Boolean)

abstract class TestCluster(
                            epmLatency: Int = 2,
                            edmLatency: Int = 2,
                          )(implicit p: Parameters) extends Module {
  val k = p(KLASE32ParamKey)

  val epm = IO(new EpmIntf())
  val irq = IO(Input(new Interrupt))
  val edm = IO(new EdmIntf())

  val xcpt = new HeartXcpt
  val noXcpt = xcpt.Lit(_.loc -> false.B, _.ma -> false.B, _.pf -> false.B, _.gf -> false.B, _.ae -> false.B)

  val mem = new ClusterTestMem(
    epmLatency,
    edmLatency,
  )(this)

//  val epmGntResp = RegNext(epm.gnt)
//  val epmAckResp0 = RegNext(epm.ack)
//  val epmAckResp = RegNext(epmAckResp0)
//  val epmDataResp = RegNext(epm.data)
//  val epmXcptResp = RegNext(epm.xcpt)
//  val edmLdAckResp = RegNext(edm.ld_ack)
//  val edmLdRdataResp = RegNext(edm.ld_rdata)
//  val edmStAckResp = RegNext(edm.st_ack)
//
//  val epmRespDelayed = Wire(new EpmIntf)
//
//  epm.req := epmRespDelayed.req
//  epm.cmd := epmRespDelayed.cmd
//  epm.addr := epmRespDelayed.addr
//  epm.kill := epmRespDelayed.kill
//  epm.flush := epmRespDelayed.flush
//  epmRespDelayed.gnt := epm.gnt
//  epmRespDelayed.ack := epm.ack
//  epmRespDelayed.bootAddr := epm.bootAddr
//  epmRespDelayed.data := epm.data
//  epmRespDelayed.xcpt := epm.xcpt

  var interrupt = TestInterrupt(false, false, false)

  def extInterrupt(v: Boolean = true) = interrupt.e = v
  def timerInterrupt(v: Boolean = true) = interrupt.t = v
  def softwareInterrupt(v: Boolean = true) = interrupt.s = v
  def checkInterruptEnd() = {
    Seq(
      (0x70000004L, () => interrupt.e = false),
      (0x70000008L, () => interrupt.t = false),
      (0x7000000CL, () => interrupt.s = false),
    ).map { case(a, f) => {
      if (mem.load(a) != BigInt(0)) {
        f()
        mem.store(a, 0x0)
      }
    }
    }
  }

  def tick(iter: Int = 1) = {
    for (i <- 0 until iter) {
      checkInterruptEnd()
      irq.e.poke(interrupt.e)
      irq.t.poke(interrupt.t)
      irq.s.poke(interrupt.s)
      mem.tick()
      clock.step()
    }
  }

  def isDone() = mem.load(0x70000000L) != BigInt(0)

  def run() = {
     while (!isDone()) tick()
    // println("Done!")
//    tick(40)
  }
}

class ClusterTestMem(
                      instLatency: Int,
                      dataLatency: Int,
                    )(dut: TestCluster) extends TestMem {
  case class TestLoadRequest(
                              ld_req: Boolean,
                              ld_vaddr: BigInt,
                              ld_kill: Boolean
                            )

  case class TestStoreRequest(
                               st_req: Boolean,
                               st_paddr: BigInt,
                               st_wdata: BigInt,
                               st_mask: BigInt,
                               st_mmio: Boolean
                             )
  case class TestDataRequest(
                              cmd: BigInt,
                              load: TestLoadRequest,
                              store: TestStoreRequest
                            )

  val dataReqQueue = mut.Queue[TestDataRequest]()
  val loadReqInitialValue = TestLoadRequest(
    ld_req = false,
    ld_vaddr = 0,
    ld_kill = false
  )

  val storeReqInitialValue = TestStoreRequest(
    st_req = false,
    st_paddr = 0,
    st_wdata = 0,
    st_mask = 0,
    st_mmio = false
  )

  val dataReqInitialValue = TestDataRequest(cmd = 0, loadReqInitialValue, storeReqInitialValue)

  for (i <- 0 until dataLatency) {
    dataReqQueue.enqueue(TestDataRequest(
      cmd = 0,
      loadReqInitialValue,
      storeReqInitialValue
    ))
  }

  case class TestInstRequest(
                            req: Boolean,
                            addr: BigInt,
                            kill: Boolean,
                            flush: Boolean
                            )

  val instReqInitialValue = TestInstRequest(
    req = false, addr = 0, kill = false, flush = false
  )
  val instReqQueue = mut.Queue[TestInstRequest]()

  for (i <- 0 until instLatency) {
    instReqQueue.enqueue(instReqInitialValue)
  }

  def tick() = {
    instTick()
    dataTick()
  }

  // 0: New request, reset to epmLatency
  // 1: Latency ended
  var instCnt = 0
//  def instTick() = {
//    val req = instReqQueue.front
//    def instResp(data: BigInt, ack: Boolean, xcpt: Option[HeartXcpt]) = {
//      dut.epm.data.poke(data)
//      dut.epm.ack.poke(ack)
//      dut.epm.xcpt.poke(dut.noXcpt)
//    }
//    def noResp() = instResp(0, false, None)
//
//    val addrValid = dut.epm.req.peekBoolean()
//    if (addrValid) {
//      if (instCnt == 0) {
//        instCnt = instLatency
//      } else {
//        instCnt -= 1
//      }
//
//      if (instCnt == 0) {
//        val addr = dut.epm.addr.peekInt().toLong
//        val data = load(addr)
//        //        val xcpt = loadXcpt(addr)
//        val xcpt = None
//        instResp(data, true, xcpt)
//        println(f"[Mem] Inst Request addr($addr%X) data($data%X) xcpt($xcpt)")
//      } else {
//        noResp()
//      }
//    } else {
//      noResp()
//    }
//  }

  def instTick() = {
    val req = instReqQueue.front
    if (!(req.req)) {
      dut.epm.ack.poke(false)
//      dut.epm.data.poke(0)
      dut.epm.xcpt.poke(dut.noXcpt)

      instReqQueue.dequeue()
    } else {
      val data = load(req.addr)
      println(f"[Inst] Load To addr(${req.addr}%X) data($data%X)")
      dut.epm.data.poke(data)
      dut.epm.ack.poke(true)
      dut.epm.xcpt.poke(dut.noXcpt)

      instReqQueue.dequeue()
    }



    if (instReqQueue.size == instLatency - 1) {
      if (dut.epm.req.peekBoolean()) {
        instReqQueue.enqueue(TestInstRequest(
          req = dut.epm.req.peekBoolean(),
          addr = dut.epm.addr.peekInt(),
          kill = false,
          flush = dut.epm.flush.peekBoolean()
        ))
      }
      else {
        instReqQueue.enqueue(instReqInitialValue)
      }
    }
    println(instReqQueue)
  }

  def dataTick() = {
    val req = dataReqQueue.front
    if (! (req.load.ld_req || req.store.st_req)) {
      dut.edm.ld_ack.poke(false)
      dut.edm.ld_rdata.poke(0)

      dut.edm.st_ack.poke(false)
      dut.edm.xcpt.poke(dut.noXcpt)

      dataReqQueue.dequeue()
    } else if (req.load.ld_req) {
      val ld_data = load(req.load.ld_vaddr)
      println(f"[Mem] Load To addr(${req.load.ld_vaddr}%X) data($ld_data%X)")
      dut.edm.ld_rdata.poke(ld_data)
      dut.edm.ld_ack.poke(true)
      dut.edm.xcpt.poke(dut.noXcpt)

      dataReqQueue.dequeue()
    }
    else {
      storeStrb(req.store.st_paddr, req.store.st_wdata, req.store.st_mask)
      dut.edm.st_ack.poke(true)
      dut.edm.xcpt.poke(dut.noXcpt)

      dataReqQueue.dequeue()
    }


    if (dataReqQueue.size == dataLatency - 1) {
      val ldReqValid = dut.edm.ld_req.peekBoolean()
      val stReqValid = dut.edm.st_req.peekBoolean()
      if (ldReqValid) {
        dataReqQueue.enqueue(TestDataRequest(
          cmd = dut.edm.cmd.peekInt(),
          TestLoadRequest(
            ld_req = dut.edm.ld_req.peekBoolean(),
            ld_vaddr = dut.edm.ld_vaddr.peekInt(),
            ld_kill = false
          ),
          storeReqInitialValue
        ))
      } else if (stReqValid) {
        dataReqQueue.enqueue(TestDataRequest(
          cmd = dut.edm.cmd.peekInt(),
          loadReqInitialValue,
          TestStoreRequest(
            st_req = dut.edm.st_req.peekBoolean(),
            st_paddr = dut.edm.st_paddr.peekInt(),
            st_wdata = dut.edm.st_wdata.peekInt(),
            st_mask = dut.edm.st_mask.peekInt(),
            st_mmio = false
          )
        ))
      }
      else {
        dataReqQueue.enqueue(dataReqInitialValue)
      }
    }
    println(dataReqQueue)
  }

  def storeStrb(addr: BigInt, data: BigInt, strb: BigInt) = {
    for (i <- 0 until 2) {
      val curAddr = addr + i*4
      val curOrigin = load(curAddr)
      val curData = data >> (i*32)
      val curStrb = (strb >> (i*4)) & 0xF
      if (curStrb != 0) {
        val v = Seq.tabulate(4) { j =>
          val dataMask = BigInt(0xFF) << (j * 8)
          if ((curStrb & (1 << j)) != 0) curData & dataMask
          else curOrigin & dataMask
        }.reduce(_ | _)
        store(curAddr, v)
        println(f"[Mem] Store To addr($curAddr%X) data($v%X)")
      }
    }
  }
}
