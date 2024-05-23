package klase32

import chisel3._
import chiseltest._
import klas.KlasTest
import klase32.config._
import klase32.param.{DefaultConfig, KLASE32ParamKey}

class RegisterFileTest extends KlasTest {
  behavior of "RegisterFile"

  it should "correctly handle read and write operations" in {
    implicit val p: Parameters = new DefaultConfig() // Ensure this matches your configuration class
    val k = p(KLASE32ParamKey)

    test(new RegisterFile) { dut =>
      // Helper function to perform a write
      def writeRegister(index: Int, data: BigInt): Unit = {
        dut.io.wp(index).valid.poke(true.B)
        dut.io.wp(index).bits.addr.poke(index.U)
        dut.io.wp(index).bits.data.poke(data.U)
        dut.clock.step(1) // Advance the clock to process the write
        dut.io.wp(index).valid.poke(false.B) // Deassert write valid
      }

      // Helper function to read and check a register
      def readAndCheck(index: Int, expectedData: BigInt): Unit = {
        dut.io.rp(index).addr.poke(index.U)
        dut.clock.step(1) // Advance the clock to ensure read occurs
        assert(dut.io.rp(index).data.peek().litValue == expectedData, s"Register $index did not have expected value $expectedData")
      }

      // Reset the register file
      dut.reset.poke(true.B)
      dut.clock.step(1)
      dut.reset.poke(false.B)

      // Test Writing to and Reading from registers
      writeRegister(0, 0x12345678L)
      writeRegister(1, 0x9abcdef0L)
      readAndCheck(0, 0x12345678L)
      readAndCheck(1, 0x9abcdef0L)

      // Check zero register is always zero
      readAndCheck(0, 0)

      // Test Writing to zero register has no effect
      writeRegister(0, 0xFFFFFFFFL)
      readAndCheck(0, 0)

      // Test all registers
      for (addr <- 1 until k.core.regNum) {
        val testData = addr * 2
        writeRegister(addr, testData)
        readAndCheck(addr, testData)
      }
    }
  }
}
