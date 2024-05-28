package klase32

import chisel3._
import chiseltest._
import klas.KlasTest
import klase32.config._
import klase32.param.DefaultConfig
import snitch.enums.DataSize
import scala.util.Random

class LSUTest extends KlasTest {
  behavior of "LSU"

  it should "correctly handle different load and store operations" in {
    implicit val p: Parameters = new DefaultConfig() // Ensure this matches your configuration class

    test(new LSU) { dut =>
      // Function to perform store operation
      def performStore(addr: BigInt, data: BigInt, size: DataSize.Type): Unit = {
        println(s"store: 0x${addr.toInt.toHexString}, 0x${data.toInt.toHexString}, $size")
        val alignedAddr = ((addr >> 2) << 2) // Align address to 4 bytes
        // val offset = addr.asUInt(1, 0).litValue.toInt
        val offset = (addr & 0x3).toInt
        val expectedMask = size match {
          case DataSize.Byte => (1 << offset)
          case DataSize.HalfWord => (3 << offset)
          case DataSize.Word => 15
        }
        dut.io.addr.poke(addr)
        dut.io.wrdata.poke(data)
        dut.io.lsuctrlIE.lsSize.poke(size)
        dut.io.lsuctrlIE.isStore.poke(StoreControl.EN)

        dut.io.storeFull.expect(false)
        dut.io.edm.st_req.expect(true)
        dut.io.edm.st_paddr.expect(alignedAddr)
        dut.io.edm.st_mask.expect(expectedMask)
        dut.io.lsuctrlIE.isStore.poke(StoreControl.default)
      }

      // Function to perform load operation
      def performLoad(addr: BigInt, rdata: BigInt, size: DataSize.Type, isSigned: SignedControl.Type): Unit = {
        println(s"load: 0x${addr.toInt.toHexString}, 0x${rdata.toInt.toHexString}, $size, $isSigned")
        val alignedAddr = ((addr >> 2) << 2) // Align address to 4 bytes
        val offset = addr.asUInt(1, 0).litValue.toInt
        val shiftedData = rdata >> (offset << 3)
        val slicedData = size match {
          case DataSize.Byte => shiftedData & 0xFF
          case DataSize.HalfWord => shiftedData & 0xFFFF
          case DataSize.Word => shiftedData
        }
        println(size)
        println(slicedData)

        // Check request
        dut.io.addr.poke(addr)
        dut.io.lsuctrlIE.isLoad.poke(LoadControl.EN)
        dut.io.edm.ld_req.expect(true)
        dut.io.edm.ld_vaddr.expect(alignedAddr)
        dut.clock.step(1)

        // Response is 1 cycle delayed
        val signedData = if (isSigned == SignedControl.signed) slicedData & 0xFFFFFFFF else slicedData
        dut.io.lsuctrlME.lsSize.poke(size)
        dut.io.lsuctrlME.isSigned.poke(isSigned)
        dut.io.edm.ld_ack.poke(true)  // DM acknowledges the load request
        dut.io.edm.ld_rdata.poke(rdata)
        dut.io.loadFull.expect(false)
        dut.io.rddata.expect(signedData)
        dut.io.lsuctrlIE.isLoad.poke(LoadControl.default)
        dut.io.edm.ld_ack.poke(false)
      }

      // Function to run random tests
      def runRandomTests(numTests: Int): Unit = {
        val rand = new Random()
        for (_ <- 0 until numTests) {
          val size = rand.nextInt(3) match {
            case 0 => DataSize.Byte
            case 1 => DataSize.HalfWord
            case 2 => DataSize.Word
          }
          val addr = size match {
            case DataSize.Byte => BigInt(rand.nextLong() & 0xFFFFFFFFL)
            case DataSize.HalfWord => BigInt(rand.nextLong() & 0xFFFFFFFCL)
            case DataSize.Word => BigInt(rand.nextLong() & 0xFFFFFFF0L) 
          }
          val data = rand.nextLong() & 0xFFFFFFFFL
          val isSigned = SignedControl(rand.nextBoolean().B)

          // Perform store
          performStore(addr, data, size)

          // Generate a random 32-bit data for the load
          val rdata = rand.nextLong() & 0xFFFFFFFFL

          // Perform load
          performLoad(addr, rdata, size, isSigned)
        }
      }

      val testAddrs = Seq(0x1000, 0x1001, 0x1002, 0x1004)
      val testData = Seq(0xAB, 0x1234, 0x5678ABCD)
      val sizes = Seq(DataSize.Byte, DataSize.HalfWord, DataSize.Word)
      val signedOptions = Seq(SignedControl.signed, SignedControl.unsigned)

      for (addr <- testAddrs) {
        for (data <- testData) {
          for (size <- sizes) {
            for (isSigned <- signedOptions) {
              performStore(addr, data, size)
            }
          }
        }
      }

      for (addr <- testAddrs) {
        for (size <- sizes) {
          for (isSigned <- signedOptions) {
            val rdata = 0xF678ABCDL // 32-bit data that will be used for the load
            performLoad(addr, rdata, size, isSigned)
          }
        }
      }

      // Run random tests
      runRandomTests(100)

    }
  }
}
