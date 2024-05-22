package klase32

import chisel3._
import chiseltest._
import klas.KlasTest
import klase32.config._
import klase32.param.DefaultConfig
import snitch.enums.DataSize

class LSUTest extends KlasTest {
  behavior of "LSU"

  it should "correctly handle different load and store operations" in {
    implicit val p: Parameters = new DefaultConfig() // Ensure this matches your configuration class

    test(new LSU) { lsu =>
      // Function to drive tests for load/store operations
      def testLoadStore(isLoad: Boolean, isStore: Boolean, size: DataSize.Type, signed: SignedControl.Type, addr: BigInt, wrdata: BigInt): Unit = {
        lsu.io.lsuctrlIE.isLoad.poke(if (isLoad) LoadControl.EN else LoadControl.default)
        lsu.io.lsuctrlIE.isStore.poke(if (isStore) StoreControl.EN else StoreControl.default)
        lsu.io.lsuctrlIE.lsSize.poke(size)
        lsu.io.lsuctrlIE.isSigned.poke(signed)
        lsu.io.addr.poke(addr.U)
        lsu.io.wrdata.poke(wrdata.U)
        lsu.clock.step(1)

        // Check outputs based on whether it's a load or store
        if (isLoad) {
          println(s"Read data for address $addr: ${lsu.io.rddata.peek().litValue}")
        }
        if (isStore) {
          println(s"Write data for address $addr with size $size: $wrdata")
        }
      }

      // Example tests
      // Test a byte load
      testLoadStore(isLoad = true, isStore = false, size = DataSize.Byte, signed = SignedControl.signed, addr = 0x1000, wrdata = 0)

      // Test a word store
      testLoadStore(isLoad = false, isStore = true, size = DataSize.Word, signed = SignedControl.unsigned, addr = 0x1004, wrdata = 0x12345678)

      // Test handling of a store followed by a load to the same address
      testLoadStore(isLoad = false, isStore = true, size = DataSize.HalfWord, signed = SignedControl.unsigned, addr = 0x1008, wrdata = 0xABCD)
      testLoadStore(isLoad = true, isStore = false, size = DataSize.HalfWord, signed = SignedControl.signed, addr = 0x1008, wrdata = 0)

      // Further tests can cover edge cases, alignment issues, etc.
    }
  }
}
