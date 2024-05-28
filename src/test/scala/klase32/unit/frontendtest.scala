package klase32

import chisel3._
import chiseltest._
import klas.KlasTest
import klase32.param.DefaultConfig
import chisel3.experimental.BundleLiterals._


class FrontendTest extends KlasTest {
  implicit val p = new DefaultConfig
  behavior of "Frontend"

  it should "correctly handle control flows and fetch operations" in {
    test(new Frontend) { dut =>
      // Initialize all input signals to 0
      dut.io.ctrl.poke(FrontendControlIE.default)
      dut.io.evec.poke(0)
      dut.io.cnd.poke(false)
      dut.io.exception.poke(false)
      dut.io.eret.poke(false)
      dut.io.divBusy.poke(false)
      dut.io.stall.poke(false)
      dut.io.aluR.poke(0)
      dut.io.flushEn.poke(IcacheFlushIE.default)
      dut.io.epm.bootAddr.poke(0)
      dut.io.epm.data.poke(0)
      dut.io.epm.xcpt.poke(chiselTypeOf(dut.io.epm.xcpt).Lit(
        _.loc -> false.B,
        _.ma -> false.B,
        _.pf -> false.B,
        _.gf -> false.B,
        _.ae -> false.B
      ))

      // Set initial fetchPC to boot address
      val bootAddr = 0x1000
      dut.io.epm.bootAddr.poke(bootAddr)

      // Cycle 1: Reset active
      dut.reset.poke(true.B)
      dut.clock.step(1)
      dut.reset.poke(false.B)
      dut.io.epm.req.expect(true)
      dut.io.epm.addr.expect(bootAddr)

      // Cycle 2: Normal fetch
      dut.clock.step(1)
      dut.io.epm.ack.poke(true) // ACK from epm
      dut.io.if_pc.expect(bootAddr + 4) // Next PC after boot address
      dut.io.epm.req.expect(true) // EPM request should be true
      dut.io.epm.addr.expect(bootAddr + 4)

      // Cycle 3: Exception handling
      dut.io.exception.poke(true)
      dut.io.evec.poke(0x2000) // Exception vector address
      dut.clock.step(1)
      dut.io.if_pc.expect(0x2000)
      dut.io.epm.req.expect(true) // EPM request should be true after exception
      dut.io.epm.addr.expect(0x2000)
      dut.io.exception.poke(false)

      // Cycle 4: Jump handling (JAL)
      dut.io.ctrl.poke(FrontendControlIE.JAL)
      dut.io.aluR.poke(0x3000) // Jump address
      dut.io.cnd.poke(true)
      dut.clock.step(1)
      dut.io.if_pc.expect(0x3000)
      dut.io.epm.req.expect(true) // EPM request should be true after jump
      dut.io.epm.addr.expect(0x3000)

      // Cycle 5: Branch handling (BR)
      dut.io.ctrl.poke(FrontendControlIE.BR)
      dut.io.aluR.poke(0x4000) // Branch target address
      dut.io.cnd.poke(true)    // Condition met
      dut.clock.step(1)
      dut.io.if_pc.expect(0x4000)
      dut.io.epm.req.expect(true) // EPM request should be true after branch
      dut.io.epm.addr.expect(0x4000)

      // Cycle 6: JALR handling
      dut.io.ctrl.poke(FrontendControlIE.JALR)
      dut.io.aluR.poke(0x5000) // JALR target address
      dut.clock.step(1)
      dut.io.if_pc.expect(0x5000)
      dut.io.epm.req.expect(true) // EPM request should be true after JALR
      dut.io.epm.addr.expect(0x5000)

      // Cycle 7: Normal fetch with stall
      dut.io.stall.poke(true)
      dut.clock.step(1)
      dut.io.epm.req.expect(false) // EPM request should be false during stall
      dut.io.stall.poke(false)

      // Cycle 8: Normal fetch with issue
      dut.io.epm.data.poke(0x12345678)
      dut.io.epm.ack.poke(true)
      dut.clock.step(1)
      dut.io.instPacket.inst.expect(0x12345678)
      dut.io.epm.req.expect(true)
      dut.io.epm.addr.expect(bootAddr + 8) // Fetch address should be updated correctly

      // Cycle 9: Handling eret
      dut.io.eret.poke(true)
      dut.io.evec.poke(0x6000)
      dut.clock.step(1)
      dut.io.if_pc.expect(0x6000)
      dut.io.epm.req.expect(true) // EPM request should be true after eret
      dut.io.epm.addr.expect(0x6000)
      dut.io.eret.poke(false)

    }
  }
}
