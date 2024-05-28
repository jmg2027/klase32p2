package klase32

import chisel3._
import chiseltest._
import klas.KlasTest
import klase32.config._
import klase32.param.{DefaultConfig, KLASE32ParamKey}
import scala.util.Random

class RegisterFileTest extends KlasTest {
  behavior of "RegisterFile"

  it should "correctly handle read and write operations" in {
    implicit val p: Parameters = new DefaultConfig() // Ensure this matches your configuration class
    val k = p(KLASE32ParamKey)

    test(new RegisterFile) { dut =>
      // Helper function to perform a write
      def writeRegister(addr: Int, index: Int, data: BigInt): Unit = {
        dut.io.wp(index).valid.poke(true)
        dut.io.wp(index).bits.addr.poke(addr)
        dut.io.wp(index).bits.data.poke(data)
        dut.clock.step(1) // Advance the clock to process the write
        dut.io.wp(index).valid.poke(false) // Deassert write valid
      }

      // Helper function to read and check a register
      def readAndCheck(addr: Int, index: Int, expectedData: BigInt): Unit = {
        dut.io.rp(index).addr.poke(addr)
        dut.clock.step(1) // Advance the clock to ensure read occurs
        assert(dut.io.rp(index).data.peek().litValue == expectedData, s"\nRegister $index did not have expected value $expectedData")
      }

      // Reset the register file
      dut.reset.poke(true.B)
      dut.clock.step(1)
      dut.reset.poke(false.B)

      // Test Writing to and Reading from registers
      writeRegister(1, 0, 0x12345678L)
      writeRegister(2, 1, 0x9abcdef0L)
      readAndCheck(1, 0, 0x12345678L)
      readAndCheck(2, 1, 0x9abcdef0L)

      // Check zero register is always zero
      readAndCheck(0, 0, 0)

      // Test Writing to zero register has no effect
      writeRegister(0, 0, 0xFFFFFFFFL)
      readAndCheck(0, 0, 0)
      writeRegister(0, 1, 0xFFFFFFFFL)
      readAndCheck(0, 1, 0)

      // Test all registers
      for (addr <- 1 until k.core.regNum) {
        for (index <- 0 until k.core.readportNum) {
          val testData = Random.nextLong(0xFFFFFFFFL)
          writeRegister(addr, index, testData)
          readAndCheck(addr, index, testData)
        }
      }
    }
  }
}
