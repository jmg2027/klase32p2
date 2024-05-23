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
      def performStore(addr: UInt, data: UInt, size: DataSize.Type): Unit = {
        val alignedAddr = (addr >> 2) << 2 // Align address to 4 bytes
        val offset = addr(1, 0)
        val expectedMask = size match {
          case DataSize.Byte => ("b0001".U << offset)
          case DataSize.HalfWord => ("b0011".U << offset)
          case DataSize.Word => "b1111".U
        }
        dut.io.addr.poke(addr)
        dut.io.wrdata.poke(data)
        dut.io.lsuctrlIE.lsSize.poke(size)
        dut.io.lsuctrlIE.isStore.poke(StoreControl.EN)

        dut.io.storeFull.expect(false.B)
        dut.io.edm.st_req.expect(true.B)
        dut.io.edm.st_paddr.expect(alignedAddr.asUInt)
        dut.io.edm.st_mask.expect(expectedMask.asUInt)
        dut.io.lsuctrlIE.isStore.poke(StoreControl.default)
      }

      // Function to perform load operation
      def performLoad(addr: UInt, rdata: UInt, size: DataSize.Type, isSigned: SignedControl.Type): Unit = {
        val alignedAddr = ((addr >> 2).asUInt << 2).asUInt // Align address to 4 bytes
        val offset = addr(1, 0)
        val shiftedData = rdata >> (offset << 3.U).asUInt
        val slicedData = size match {
          case DataSize.Byte => shiftedData(7, 0)
          case DataSize.HalfWord => shiftedData(15, 0)
          case DataSize.Word => shiftedData
        }

        dut.io.addr.poke(addr)
        dut.io.lsuctrlIE.lsSize.poke(size)
        dut.io.lsuctrlIE.isLoad.poke(LoadControl.EN)
        dut.io.lsuctrlIE.isSigned.poke(isSigned)
        dut.io.edm.ld_req.expect(true.B)
        dut.io.edm.ld_vaddr.expect(alignedAddr)
        dut.clock.step(1)

        val signedData = if (isSigned == SignedControl.signed) slicedData.asSInt else slicedData.asUInt
        dut.io.edm.ld_ack.poke(true.B)  // DM acknowledges the load request
        dut.io.edm.ld_rdata.poke(rdata)
        dut.io.loadFull.expect(false.B)
        dut.io.rddata.expect(signedData.asUInt)
        dut.io.lsuctrlIE.isLoad.poke(LoadControl.default)
        dut.io.edm.ld_ack.poke(false.B)
      }

      // Function to run random tests
      def runRandomTests(numTests: Int): Unit = {
        val rand = new Random()
        for (_ <- 0 until numTests) {
          val addr = (rand.nextLong() & 0xFFFFFFFFL).U
          val data = rand.nextInt().U
          val size = rand.nextInt(3) match {
            case 0 => DataSize.Byte
            case 1 => DataSize.HalfWord
            case 2 => DataSize.Word
          }
          val isSigned = SignedControl(rand.nextBoolean().B)

          // Perform store
          performStore(addr, data, size)

          // Generate a random 32-bit data for the load
          val rdata = rand.nextInt().U

          // Perform load
          performLoad(addr, rdata, size, isSigned)
        }
      }

      val testAddrs = Seq(0x1000.U, 0x1001.U, 0x1002.U, 0x1003.U, 0x1004.U)
      val testData = Seq(0xAB.U, 0x1234.U, 0x5678ABCD.U)
      val sizes = Seq(DataSize.Byte, DataSize.HalfWord, DataSize.Word)
      val signedOptions = Seq(SignedControl.signed, SignedControl.unsigned)

      for (addr <- testAddrs) {
        for (data <- testData) {
          for (size <- sizes) {
            for (isSigned <- signedOptions) {
              performStore(addr, data, size)

              val rdata = 0x5678ABCD.U // 32-bit data that will be used for the load
              performLoad(addr, rdata, size, isSigned)
            }
          }
        }
      }

      // Run random tests
      runRandomTests(100)

    }
  }
}
