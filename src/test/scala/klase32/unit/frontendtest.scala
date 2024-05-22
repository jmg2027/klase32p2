package klase32

import chisel3._
import chiseltest._
import klas.KlasTest
import klase32.config._
import klase32.param.DefaultConfig

class FrontendTest extends KlasTest {
  implicit val p = new DefaultConfig
  behavior of "Frontend"

  it should "correctly handle control flows and fetch operations" in {
    test(new Frontend) { c =>
            // Initialize all input signals to 0
      dut.io.ctrl.poke(0.U)
      dut.io.if_pc.poke(0.U)
      dut.io.evec.poke(0.U)
      dut.io.cnd.poke(false.B)
      dut.io.exception.poke(false.B)
      dut.io.eret.poke(false.B)
      dut.io.divBusy.poke(false.B)
      dut.io.stall.poke(false.B)
      dut.io.aluR.poke(0.U)
      dut.io.flushEn.poke(0.U.asTypeOf(new IcacheFlushIE))
      dut.io.epm.bootAddr.poke(0.U)
      dut.io.epm.data.poke(0.U)
      dut.io.epm.xcpt.poke(0.U.asTypeOf(new HeartXcpt))

      // Set initial fetchPC to boot address
      val bootAddr = 0x1000
      dut.io.epm.bootAddr.poke(bootAddr.U)

      // Cycle 1: Reset active
      dut.reset.poke(true.B)
      dut.clock.step(1)
      dut.reset.poke(false.B)
      dut.io.epm.req.expect(false.B)
      dut.io.epm.addr.expect(bootAddr.U)

      // Cycle 2: Normal fetch
      dut.io.epm.ack.poke(true.B) // ACK from epm
      dut.clock.step(1)
      dut.io.pcRegWrite.valid.expect(true.B)
      dut.io.pcRegWrite.bits.expect((bootAddr + 4).U) // Next PC after boot address
      dut.io.epm.req.expect(true.B) // EPM request should be true
      dut.io.epm.addr.expect(bootAddr.U + 4.U)

      // Cycle 3: Exception handling
      dut.io.exception.poke(true.B)
      dut.io.evec.poke(0x2000.U) // Exception vector address
      dut.clock.step(1)
      dut.io.pcRegWrite.valid.expect(true.B)
      dut.io.pcRegWrite.bits.expect(0x2000.U)
      dut.io.epm.req.expect(true.B) // EPM request should be true after exception
      dut.io.epm.addr.expect(0x2000.U)
      dut.io.exception.poke(false.B)

      // Cycle 4: Jump handling (JAL)
      dut.io.ctrl.poke(FrontendControlIE.JAL)
      dut.io.aluR.poke(0x3000.U) // Jump address
      dut.io.cnd.poke(true.B)
      dut.clock.step(1)
      dut.io.pcRegWrite.valid.expect(true.B)
      dut.io.pcRegWrite.bits.expect(0x3000.U)
      dut.io.epm.req.expect(true.B) // EPM request should be true after jump
      dut.io.epm.addr.expect(0x3000.U)
      dut.io.ctrl.poke(0.U)

      // Cycle 5: Branch handling (BR)
      dut.io.ctrl.poke(FrontendControlIE.BR)
      dut.io.aluR.poke(0x4000.U) // Branch target address
      dut.io.cnd.poke(true.B)    // Condition met
      dut.clock.step(1)
      dut.io.pcRegWrite.valid.expect(true.B)
      dut.io.pcRegWrite.bits.expect(0x4000.U)
      dut.io.epm.req.expect(true.B) // EPM request should be true after branch
      dut.io.epm.addr.expect(0x4000.U)
      dut.io.ctrl.poke(0.U)

      // Cycle 6: JALR handling
      dut.io.ctrl.poke(FrontendControlIE.JALR)
      dut.io.aluR.poke(0x5000.U) // JALR target address
      dut.clock.step(1)
      dut.io.pcRegWrite.valid.expect(true.B)
      dut.io.pcRegWrite.bits.expect(0x5000.U)
      dut.io.epm.req.expect(true.B) // EPM request should be true after JALR
      dut.io.epm.addr.expect(0x5000.U)
      dut.io.ctrl.poke(0.U)

      // Cycle 7: Normal fetch with stall
      dut.io.stall.poke(true.B)
      dut.clock.step(1)
      dut.io.pcRegWrite.valid.expect(false.B)
      dut.io.epm.req.expect(false.B) // EPM request should be false during stall
      dut.io.stall.poke(false.B)

      // Cycle 8: Normal fetch with issue
      dut.io.epm.data.poke(0x12345678.U)
      dut.io.epm.ack.poke(true.B)
      dut.clock.step(1)
      dut.io.pcRegWrite.valid.expect(true.B)
      dut.io.instPacket.inst.expect(0x12345678.U)
      dut.io.epm.req.expect(true.B)
      dut.io.epm.addr.expect((bootAddr + 8).U) // Fetch address should be updated correctly

      // Cycle 9: Handling eret
      dut.io.eret.poke(true.B)
      dut.io.evec.poke(0x6000.U)
      dut.clock.step(1)
      dut.io.pcRegWrite.valid.expect(true.B)
      dut.io.pcRegWrite.bits.expect(0x6000.U)
      dut.io.epm.req.expect(true.B) // EPM request should be true after eret
      dut.io.epm.addr.expect(0x6000.U)
      dut.io.eret.poke(false.B)

    }
  }
}
