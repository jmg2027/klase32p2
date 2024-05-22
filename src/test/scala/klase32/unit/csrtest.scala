package klase32

import chisel3._
import chiseltest._
import klas.KlasTest
import klase32.config._
import klase32.param.DefaultConfig
import klase32.CSR.CSRAddr

class CSRModuleTest extends KlasTest {
  behavior of "CSRModule"

  it should "correctly handle read and write operations for various CSRs" in {
    implicit val p: Parameters = new DefaultConfig() // Ensure this matches your configuration class

    test(new CSRModule) { dut =>
      // Function to test CSR read and write operations
          test(new CSRTest) { c =>
      def testCSRField(addr: UInt, fieldName: String, default: UInt) {
        // Initial read should be the default value
        c.io.ctrl.addr.poke(addr)
        c.io.ctrl.inst.poke(CSRControl.RW)
        c.io.ctrl.in.poke(default)
        c.clock.step(1)
        c.io.rd.expect(default)

        // Write to the CSR field using CSRRW
        val newValue = 0xdeadbeefL.U
        c.io.ctrl.in.poke(newValue)
        c.io.ctrl.inst.poke(CSRControl.RW)
        c.clock.step(1)
        c.io.rd.expect(default) // Should read old value before write
        c.io.ctrl.inst.poke(CSRControl.RS)
        c.io.rd.expect(newValue) // After write, it should reflect the new value

        // Set up another value to test CSRRS (set bits)
        val setBits = 0x00000001L.U
        c.io.ctrl.in.poke(setBits)
        c.io.ctrl.inst.poke(CSRControl.RS)
        c.clock.step(1)
        c.io.rd.expect(newValue) // RS should read the old value
        c.io.ctrl.inst.poke(CSRControl.RS)
        c.io.rd.expect(newValue | setBits) // Set bits operation

        // Set up another value to test CSRRC (clear bits)
        val clearBits = 0x00000001L.U
        c.io.ctrl.in.poke(clearBits)
        c.io.ctrl.inst.poke(CSRControl.RC)
        c.clock.step(1)
        c.io.rd.expect(newValue | setBits) // RC should read the current value
        c.io.ctrl.inst.poke(CSRControl.RC)
        c.io.rd.expect((newValue | setBits) & (~clearBits).asUInt) // Clear bits operation
      }

      // Test for each CSR field
      testCSRField(CSR.CSRAddr.mstatus.U, "mstatus", 0.U)
      testCSRField(CSR.CSRAddr.mstatush.U, "mstatush", 0.U)
      testCSRField(CSR.CSRAddr.misa.U, "misa", 0.U)
      testCSRField(CSR.CSRAddr.medeleg.U, "medeleg", 0.U)
      testCSRField(CSR.CSRAddr.mideleg.U, "mideleg", 0.U)
      testCSRField(CSR.CSRAddr.mie.U, "mie", 0.U)
      testCSRField(CSR.CSRAddr.mtvec.U, "mtvec", 0.U)
      testCSRField(CSR.CSRAddr.mvendorid.U, "mvendorid", 0.U)
      testCSRField(CSR.CSRAddr.marchid.U, "marchid", 0.U)
      testCSRField(CSR.CSRAddr.mimpid.U, "mimpid", 0.U)
      testCSRField(CSR.CSRAddr.mhartid.U, "mhartid", 0.U)
      testCSRField(CSR.CSRAddr.mscratch.U, "mscratch", 0.U)
      testCSRField(CSR.CSRAddr.mepc.U, "mepc", 0.U)
      testCSRField(CSR.CSRAddr.mcause.U, "mcause", 0.U)
      testCSRField(CSR.CSRAddr.mtval.U, "mtval", 0.U)
      testCSRField(CSR.CSRAddr.mip.U, "mip", 0.U)
      testCSRField(CSR.CSRAddr.mconfigptr.U, "mconfigptr", 0.U)
      testCSRField(CSR.CSRAddr.menvcfg.U, "menvcfg", 0.U)
      testCSRField(CSR.CSRAddr.menvcfgh.U, "menvcfgh", 0.U)
      testCSRField(CSR.CSRAddr.mseccfg.U, "mseccfg", 0.U)
    }


      // Exception handling
      dut.io.exception.poke(true.B)
      dut.io.cause.poke(0x04.U) // Example cause
      dut.io.epc.poke(0x1000.U)
      dut.io.ctrl.addr.poke(CSR.CSRAddr.mcause) // Testing exception capture
      dut.clock.step(1)
      dut.io.rd.expect(0x04.U) // Check if mcause was correctly captured

      // Interrupt handling
      c.io.interrupt.e.poke(true.B)
      c.io.interrupt.t.poke(true.B)
      c.io.interrupt.s.poke(true.B)
      c.clock.step(1)
      c.io.interruptPending.expect(true.B)
      c.io.interruptCause.expect(11.U) // Check if interrupt cause was correctly captured

    }
  }
}