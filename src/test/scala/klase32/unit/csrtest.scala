package klase32

import chisel3._
import chiseltest._
import klas.KlasTest
import klase32.config._
import klase32.param.DefaultConfig

class CSRModuleTest extends KlasTest {
  behavior of "CSRModule"

  it should "correctly handle read and write operations for various CSRs" in {
    implicit val p: Parameters = new DefaultConfig() // Ensure this matches your configuration class

    test(new CSRModule) { dut =>
      // Function to test CSR read and write operations
      def testCSRRW(addr: UInt, value: UInt): Unit = {
        dut.io.ctrl.in.poke(value)
        dut.io.ctrl.inst.poke(CSRControl.RW)
        dut.clock.step(1)
      }

      def testCSRField(addr: UInt, newValue: UInt) {
        val default = 0.U
        // Initial read should be the default value
        testCSRRW(addr, default)
        dut.io.rd.expect(default)

        // Write to the CSR field using CSRRW
        testCSRRW(addr, newValue)
        dut.io.rd.expect(newValue) // After write, it should reflect the new value

        // Set up another value to test CSRRS (set bits)
        val setBits = 0x00000001L.U
        dut.io.ctrl.in.poke(setBits)
        dut.io.ctrl.inst.poke(CSRControl.RS)
        dut.io.rd.expect(newValue) // RS should read the old value
        dut.clock.step(1)
        dut.io.ctrl.inst.poke(CSRControl.RS)
        dut.io.rd.expect(newValue | setBits) // Set bits

        // Set up another value to test CSRRC (clear bits)
        val clearBits = 0x00000001L.U
        dut.io.ctrl.in.poke(clearBits)
        dut.io.ctrl.inst.poke(CSRControl.RC)
        dut.io.rd.expect(newValue | setBits) // RC should read the current value
        dut.clock.step(1)
        dut.io.rd.expect((newValue | setBits) & (~clearBits).asUInt) // Clear bits operation
      }

      // Test for each CSR field
      testCSRField(CSR.CSRAddr.mstatus.U, 1.U)
      testCSRField(CSR.CSRAddr.mstatush.U, 1.U)
      testCSRField(CSR.CSRAddr.medeleg.U, 1.U)
      testCSRField(CSR.CSRAddr.mideleg.U, 1.U)
      testCSRField(CSR.CSRAddr.mie.U, 1.U)
      testCSRField(CSR.CSRAddr.mtvec.U, 1.U)
      testCSRField(CSR.CSRAddr.mscratch.U, 1.U)
      testCSRField(CSR.CSRAddr.mepc.U, 1.U)
      testCSRField(CSR.CSRAddr.mcause.U, 1.U)
      testCSRField(CSR.CSRAddr.mtval.U, 1.U)
      testCSRField(CSR.CSRAddr.mip.U, 1.U)
      testCSRField(CSR.CSRAddr.menvcfg.U, 1.U)

      // Exception handling
      dut.io.exception.poke(true.B)
      dut.io.cause.poke(0x04.U) // Example cause
      dut.io.epc.poke(0x1000.U)
      dut.io.ctrl.addr.poke(CSR.CSRAddr.mcause) // Testing exception capture
      dut.clock.step(1)
      dut.io.rd.expect(0x04.U) // Check if mcause was correctly captured

      // Interrupt handling
      def testInterrupt(cause: String): Unit = {
        val causeInt = cause match {
          case "external" => 3
          case "timer" => 7
          case "software" => 11
        }
        testCSRRW(CSR.CSRAddr.mie.U, (1 << causeInt).asUInt)
        if (causeInt == 3) {dut.io.interrupt.e.poke(true.B)}
        else if (causeInt == 7) {dut.io.interrupt.t.poke(true.B)}
        else if (causeInt == 11) {dut.io.interrupt.s.poke(true.B)}
        else {assert(true, "Not allowed interrupt cause!")}
        dut.clock.step(1)
        dut.io.interruptPending.expect(true.B)
        dut.io.interruptCause.expect(causeInt.U) // Check if interrupt cause was correctly captured
      }
      testInterrupt("external")
      testInterrupt("timer")
      testInterrupt("software")
    }
  }
}
