package klase32.unit
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import klase32.config._
import klase32.param.KlasE32ParamKey

class CSRModuleTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "CSRModule"

  it should "correctly handle read and write operations for various CSRs" in {
    implicit val p: Parameters = new DefaultConfig() // Ensure this matches your configuration class

    test(new CSRModule) { c =>
      // Function to test CSR read and write operations
      def testCSRReadWrite(addr: UInt, writeData: BigInt, readBack: BigInt, readMask: BigInt): Unit = {
        c.io.ctrl.addr.poke(addr)
        c.io.ctrl.in.poke(writeData.U)
        c.io.ctrl.inst.poke(CSRInstMuxIE.RW) // Assuming RW is the correct instruction for a write operation
        c.clock.step(1)
        c.io.ctrl.inst.poke(CSRInstMuxIE.RW) // Change to a read operation if needed
        c.clock.step(1)
        c.io.rd.expect((readBack & readMask).U)
      }

      // Test each CSR with specific scenarios
      // You should add all relevant CSR addresses and data scenarios
      val csrAddresses = Seq(CSR.CSRAddr.mstatus, CSR.CSRAddr.misa, CSR.CSRAddr.mie)
      val testData = Seq(
        (0x300.U, 0x00000002L, 0x00000002L, 0xFFFFFFFFL), // Example data for mstatus
        (0x301.U, 0x40000000L, 0x40000000L, 0xFFFFFFFFL), // Example data for misa
        (0x304.U, 0x00000001L, 0x00000001L, 0xFFFFFFFFL)  // Example data for mie
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
