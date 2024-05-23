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
        dut.io.ctrl.addr.poke(addr)
        dut.io.ctrl.in.poke(value)
        dut.io.ctrl.inst.poke(CSRControl.RW)
        dut.clock.step(1)
      }

      def testCSRRS(addr: UInt, value: UInt): Unit = {
        dut.io.ctrl.addr.poke(addr)
        dut.io.ctrl.in.poke(value)
        dut.io.ctrl.inst.poke(CSRControl.RS)
        dut.clock.step(1)
      }

      def testCSRRC(addr: UInt, value: UInt): Unit = {
        dut.io.ctrl.addr.poke(addr)
        dut.io.ctrl.in.poke(value)
        dut.io.ctrl.inst.poke(CSRControl.RC)
        dut.clock.step(1)
      }

      def testCSRField(addr: UInt, newValue: UInt): Unit = {
        println(addr)
        val default = 0.U
        // Initial read should be the default value
        testCSRRW(addr, default)
        dut.io.rd.expect(default)
        println("default")

        // Write to the CSR field using CSRRW
        testCSRRW(addr, newValue)
        dut.io.rd.expect(newValue) // After write, it should reflect the new value
        println(newValue)
        println(dut.io.rd.peek())
        println("RW")

        // Set up another value to test CSRRS (set bits)
        val setBits = 0x00000001L.U
        dut.io.ctrl.in.poke(setBits)
        dut.io.ctrl.inst.poke(CSRControl.RS)
        println(dut.io.rd.peek())
        dut.io.rd.expect(newValue) // RS should read the old value
        dut.clock.step(1)
        dut.io.ctrl.inst.poke(CSRControl.RS)
        dut.io.rd.expect((newValue.litValue | setBits.litValue).asUInt) // Set bits
        println("RS")

        // Set up another value to test CSRRC (clear bits)
        val clearBits = 0x00000001L.U
        dut.io.ctrl.in.poke(clearBits)
        dut.io.ctrl.inst.poke(CSRControl.RC)
        dut.io.rd.expect((newValue.litValue | setBits.litValue).asUInt) // RC should read the current value
        dut.clock.step(1)
        dut.io.rd.expect(((newValue.litValue | setBits.litValue) & ~(clearBits.litValue)).asUInt) // Clear bits operation
        println("RC")
      }

      // Test for each CSR field
      testCSRField(CSR.CSRAddr.mstatus.U, 1.U)
      testCSRField(CSR.CSRAddr.mstatush.U, 1.U)
      testCSRField(CSR.CSRAddr.medeleg.U, 1.U)
      testCSRField(CSR.CSRAddr.mideleg.U, 1.U)
      testCSRField(CSR.CSRAddr.mie.U, 1.U)
      testCSRField(CSR.CSRAddr.mtvec.U, 1.U)
      testCSRField(CSR.CSRAddr.mscratch.U, 1.U)
//      testCSRField(CSR.CSRAddr.mepc.U, 1.U)
      testCSRField(CSR.CSRAddr.mcause.U, 1.U)
      testCSRField(CSR.CSRAddr.mtval.U, 1.U)
      testCSRField(CSR.CSRAddr.mip.U, 1.U)
//      testCSRField(CSR.CSRAddr.menvcfg.U, 1.U)

      // Exception handling
      println("xcpt")
      dut.io.exception.poke(true.B)
      dut.io.cause.poke(0x04.U) // Example cause
      dut.io.epc.poke(0x1000.U)
      dut.clock.step(1)
      testCSRRS(CSR.CSRAddr.mcause.U, 0.U)
      dut.io.rd.expect(0x04.U) // Check if mcause was correctly captured
      dut.io.evec.expect(0x1000.U) // Check if evec was correctly captured
      dut.clock.step(1)

      // Interrupt handling
      def testInterrupt(cause: String): Unit = {
        println(s"int {$cause}")
        val causeInt = cause match {
          case "software" => 3
          case "timer" => 7
          case "external" => 11
        }
        testCSRRW(CSR.CSRAddr.mie.U, (1 << causeInt).asUInt)
        if (causeInt == 3) {dut.io.interrupt.s.poke(true.B)}
        else if (causeInt == 7) {dut.io.interrupt.t.poke(true.B)}
        else if (causeInt == 11) {dut.io.interrupt.e.poke(true.B)}
        else {assert(throw new Exception(s"Not allowed interrupt cause! {$cause}"))}
        dut.clock.step(1)
        dut.io.interruptPending.expect(true.B)
        dut.io.interruptCause.expect(causeInt.U) // Check if interrupt cause was correctly captured
      }

      testInterrupt("software")
      testInterrupt("timer")
      testInterrupt("external")

    }
  }
}
