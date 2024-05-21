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

    test(new CSRModule) { c =>
      // Function to test CSR read and write operations
      def testCSRReadWrite(addr: Int, writeData: BigInt, readBack: BigInt, readMask: BigInt): Unit = {
        println(addr: Int, writeData: BigInt, readBack: BigInt, readMask: BigInt)
        c.io.ctrl.addr.poke(addr.U)
        c.io.ctrl.in.poke(writeData.U)
        c.io.ctrl.inst.poke(CSRControl.RW)
        c.clock.step(1)
//        println(c.io.rd.peekInt(), readBack & readMask)
        c.io.ctrl.inst.poke(CSRControl.RW)
//        println(c.io.rd.peekInt(), readBack & readMask)
        c.clock.step(1)
        println(c.io.rd.peekInt(), readBack & readMask)
        c.io.rd.expect((readBack & readMask).U)
      }

      // Test each CSR with specific scenarios
      val csrAddresses = Seq(CSR.CSRAddr.mstatus, CSR.CSRAddr.misa, CSR.CSRAddr.mie)
      val testData = Seq(
        (CSRAddr.mstatus, 0x00000002L, 0x00000002L, 0xFFFFFFFFL), // Example data for mstatus
        (CSRAddr.mie, 0x00000001L, 0x00000001L, 0xFFFFFFFFL)  // Example data for mie
      )

      for ((addr, writeData, expectedReadback, mask) <- testData) {
        testCSRReadWrite(addr, writeData, expectedReadback, mask)
      }

      // Special cases for exceptions and interrupts
      c.io.exception.poke(true.B)
      c.io.cause.poke(0x04.U) // Example cause
      c.io.epc.poke(0x1000.U)
      c.io.ctrl.addr.poke(CSR.CSRAddr.mcause) // Testing exception capture
      c.clock.step(1)
      c.io.rd.expect(0x04.U) // Check if mcause was correctly captured

      // More scenarios for interrupts, ebreak, mret, etc.
    }
  }
}
