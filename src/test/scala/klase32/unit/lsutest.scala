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
        // Function to perform store operation
        def performStore(addr: UInt, data: UInt, size: UInt): Unit = {
          poke(c.io.addr, addr)
          poke(c.io.wrdata, data)
          poke(c.io.lsuctrlIE.lsSize, size)
          poke(c.io.lsuctrlIE.isStore, true.B)
          val alignedAddr = (addr >> 2) << 2 // Align address to 4 bytes
          val offset = addr(1, 0)
          val expectedMask = size match {
            case DataSize.Byte => ("b0001".U << offset)
            case DataSize.HalfWord => ("b0011".U << offset)
            case DataSize.Word => "b1111".U
          }
          expect(c.io.storeFull, false.B)
          expect(c.io.edm.st_req, true.B)
          expect(c.io.edm.st_paddr, alignedAddr)
          expect(c.io.edm.st_mask, expectedMask)
          poke(c.io.lsuctrlIE.isStore, false.B)
        }

        // Function to perform load operation
        def performLoad(addr: UInt, rdata: UInt, size: UInt, isSigned: Bool, expectedData: UInt): Unit = {
          poke(c.io.addr, addr)
          poke(c.io.lsuctrlIE.lsSize, size)
          poke(c.io.lsuctrlIE.isLoad, true.B)
          poke(c.io.lsuctrlIE.isSigned, isSigned)
          step(1)
          val alignedAddr = (addr >> 2) << 2 // Align address to 4 bytes
          val offset = addr(1, 0)
          val shiftedData = rdata >> (offset << 3)
          val slicedData = size match {
            case DataSize.Byte => shiftedData(7, 0)
            case DataSize.HalfWord => shiftedData(15, 0)
            case DataSize.Word => shiftedData
          }
          val signedData = if (isSigned.litToBoolean) slicedData.asSInt else slicedData.asUInt
          poke(c.io.edm.ld_ack, true.B)  // DM acknowledges the load request
          poke(c.io.edm.ld_rdata, rdata)
          expect(c.io.loadFull, false.B)
          expect(c.io.rddata, signedData)
          poke(c.io.lsuctrlIE.isLoad, false.B)
          poke(c.io.edm.ld_ack, false.B)
        }

        // Function to calculate expected data based on size and offset
        def calculateExpectedData(data: UInt, addr: UInt, size: UInt, isSigned: Bool): UInt = {
          val offset = addr(1, 0)
          val expectedData = size match {
            case DataSize.Byte =>
              val byteData = (data >> (offset * 8)) & 0xFF.U
              if (isSigned.litToBoolean) byteData.zext() else byteData
            case DataSize.HalfWord =>
              val halfWordData = (data >> (offset & 1.U) * 16) & 0xFFFF.U
              if (isSigned.litToBoolean) halfWordData.zext() else halfWordData
            case DataSize.Word => data
          }
          expectedData
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
            val isSigned = rand.nextBoolean().B

            // Perform store
            performStore(addr, data, size)

            // Generate a random 32-bit data for the load
            val rdata = rand.nextInt().U
            val expectedData = calculateExpectedData(rdata, addr, size, isSigned)

            // Perform load
            performLoad(addr, rdata, size, isSigned, expectedData)
          }
        }

        val testAddrs = Seq(0x1000.U, 0x1001.U, 0x1002.U, 0x1003.U, 0x1004.U)
        val testData = Seq(0xAB.U, 0x1234.U, 0x5678ABCD.U)
        val sizes = Seq(DataSize.Byte, DataSize.HalfWord, DataSize.Word)
        val signedOptions = Seq(true.B, false.B)

        for (addr <- testAddrs) {
          for (data <- testData) {
            for (size <- sizes) {
              for (isSigned <- signedOptions) {
                performStore(addr, data, size)

                val rdata = 0x5678ABCD.U // 32-bit data that will be used for the load
                val expectedData = calculateExpectedData(rdata, addr, size, isSigned)
                performLoad(addr, rdata, size, isSigned, expectedData)
              }
            }
          }
        }

        // Run random tests
        runRandomTests(100)

    }
  }
}
