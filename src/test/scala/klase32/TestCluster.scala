////package snitch.cluster
//package klase32.cluster
////package klase32
//
//import scala.collection.{mutable => mut}
//import scala.Enumeration
//import chisel3._
//import chiseltest._
//import klase32.{EdmIntf, EpmIntf}
//
////import snitch.{interface => inf}
//import snitch.types._
//import snitch.enums._
//import klase32.config._
//
//import assembler.RISCVAssembler
////import snitch.param.SnitchParamKey
//import klase32.param.KLASE32ParamKey
//
//import snitch.test.TestMem
//
//case class TestInterrupt(var e: Boolean, var t: Boolean, var s: Boolean)
//
//abstract class TestCluster(
//                            epmLatency: Int = 1,
//                            edmLatency: Int = 1,
//                          )(implicit p: Parameters) extends Module {
//  val k = p(KLASE32ParamKey)
//
//  val epm = IO(new EpmIntf())
//  val irq = IO(Input(new Interrupt))
//  val edm = IO(new EdmIntf())
//
//  val mem = new ClusterTestMem(
//    epmLatency,
//    edmLatency,
//  )(this)
//
//  var interrupt = TestInterrupt(false, false, false)
//
//  def extInterrupt(v: Boolean = true) = interrupt.e = v
//  def timerInterrupt(v: Boolean = true) = interrupt.t = v
//  def softwareInterrupt(v: Boolean = true) = interrupt.s = v
//  def checkInterruptEnd() = {
//    Seq(
//      (0x70000004L, () => interrupt.e = false),
//      (0x70000008L, () => interrupt.t = false),
//      (0x7000000CL, () => interrupt.s = false),
//    ).map { case(a, f) => {
//      if (mem.load(a) != BigInt(0)) {
//        f()
//        mem.store(a, 0x0)
//      }
//    }
//    }
//  }
//
//  def tick(iter: Int = 1) = {
//    for (i <- 0 until iter) {
//      checkInterruptEnd()
//      interrupt.e.poke(interrupt.e)
//      interrupt.t.poke(interrupt.t)
//      interrupt.s.poke(interrupt.s)
//      mem.tick()
//      clock.step()
//    }
//  }
//
//  def isDone() = mem.load(0x70000000L) != BigInt(0)
//
//  def run() = {
//    while (!isDone()) tick()
//  }
//}
//
//class ClusterTestMem(
//                      instLatency: Int,
//                      dataLatency: Int,
//                    )(dut: TestCluster) extends TestMem {
//  case class TestDataRequest(
//                              addr: BigInt,
//                              isWrite: Boolean,
//                              data: BigInt,
//                              strb: BigInt,
//                              size: DataSize.Type,
//                              amo: AMOType.Type,
//                              valid: Boolean
//                            )
//
//  val dataReqQueue = mut.Queue[TestDataRequest]()
//  for (i <- 0 until dataLatency) {
//    dataReqQueue.enqueue(TestDataRequest(
//      addr = 0,
//      isWrite = false,
//      data = 0,
//      strb = 0,
//      size = DataSize.Byte,
//      amo = AMOType.Add,
//      valid = false
//    ))
//  }
//
//  def tick() = {
//    instTick()
//    dataTick()
//  }
//
//  // 0: New request, reset to epmLatency
//  // 1: Latency ended
//  var instCnt = 0
//  def instTick() = {
//    def instResp(data: BigInt, ack: Boolean, xcpt: Option[HeartXcpt]) = {
//      dut.epm.data.poke(data)
//      dut.epm.ack.poke(ack)
//      dut.epm.xcpt.poke(xcpt.getOrElse(false))
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
////        val xcpt = loadXcpt(addr)
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
//
//  def dataTick() = {
//    val req = dataReqQueue.front
//    if (! req.valid) {
//      dut.data.resp.bits.data.poke(0)
//      dut.data.resp.bits.error.poke(false)
//      dut.data.resp.valid.poke(false)
//
//      dataReqQueue.dequeue()
//      dut.data.req.ready.poke(true)
//    } else {
//      val addr = req.addr
//      val data = if (req.isWrite) {
//        storeStrb(addr, req.data, req.strb)
//        BigInt(0)
//      } else {
//        val v = load(addr)
//        println(f"[Mem] Load To addr($addr%X) data($v%X)")
//        v
//      }
//      dut.data.resp.bits.data.poke(data)
//      dut.data.resp.bits.error.poke(false)
//      dut.data.resp.valid.poke(true)
//
//      val respReady = dut.data.resp.ready.peekBoolean()
//      if (respReady) {
//        dataReqQueue.dequeue()
//        dut.data.req.ready.poke(true)
//      } else {
//        dut.data.req.ready.poke(false)
//      }
//    }
//
//    if (dataReqQueue.size == dataLatency - 1) {
//      val reqValid = dut.data.req.valid.peekBoolean()
//      if (reqValid) {
//        val req = dut.data.req.bits
//        dataReqQueue.enqueue(TestDataRequest(
//          addr = req.addr.peekInt(),
//          isWrite = req.isWrite.peekBoolean(),
//          data = req.data.peekInt(),
//          strb = req.strb.peekInt(),
//          size = req.size.peek(),
//          amo = req.amo.peek(),
//          valid = true
//        ))
//      } else {
//        dataReqQueue.enqueue(TestDataRequest(
//          addr = 0,
//          isWrite = false,
//          data = 0,
//          strb = 0,
//          size = DataSize.Byte,
//          amo = AMOType.Add,
//          valid = false
//        ))
//      }
//    }
//  }
//
//  def storeStrb(addr: BigInt, data: BigInt, strb: BigInt) = {
//    for (i <- 0 until 2) {
//      val curAddr = addr + i*4
//      val curOrigin = load(curAddr)
//      val curData = data >> (i*32)
//      val curStrb = (strb >> (i*4)) & 0xF
//      if (curStrb != 0) {
//        val v = Seq.tabulate(4) { j =>
//          val dataMask = BigInt(0xFF) << (j * 8)
//          if ((curStrb & (1 << j)) != 0) curData & dataMask
//          else curOrigin & dataMask
//        }.reduce(_ | _)
//        store(curAddr, v)
//        println(f"[Mem] Store To addr($curAddr%X) data($v%X)")
//      }
//    }
//  }
//}
