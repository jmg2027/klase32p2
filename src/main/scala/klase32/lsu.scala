package klase32

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.util.BitPat.bitPatToUInt
import klase32.config._
import klase32.param.KLASE32ParamKey
import snitch.enums.DataSize
import freechips.rocketchip.rocket.constants.MemoryOpConstants
import freechips.rocketchip.rocket.Causes


class LSU(implicit p: Parameters) extends CoreModule with MemoryOpConstants {
  val k = p(KLASE32ParamKey)

  val io = IO(new Bundle{
    val lsuctrlIE = Input(new LSUControl)
    val lsuctrlME = Input(new LSUControl)

    val edm = new EdmIntf()

    val addr = Input(UInt(k.addrWidth.W))
    val rddata = Output(UInt(k.dataWidth.W))
    val wrdata = Input(UInt(k.dataWidth.W))

    val storeFull = Output(Bool())
    val loadFull = Output(Bool())

    val ldXcpt = Output(new HeartXcpt)
    val stXcpt = Output(new HeartXcpt)
  }
  )
  // Align address
  val addrAligned = Cat(io.addr(k.addrWidth - 1, log2Ceil(addressAlignByte)), 0.U(log2Ceil(addressAlignByte).W))
  val offsetAligned = io.addr(log2Ceil(addressAlignByte) - 1, 0)

  // Align store data: shift the write data based on the address offset and create a mask for the store operation
  val storeDataAligned = io.wrdata << (offsetAligned << 3).asUInt
  val storeDataMask = Mux1H(
    Seq(
      (io.lsuctrlIE.lsSize === DataSize.Byte) -> ("b0001".U << offsetAligned),
      (io.lsuctrlIE.lsSize === DataSize.HalfWord) -> ("b0011".U << offsetAligned),
      (io.lsuctrlIE.lsSize === DataSize.Word) -> "b1111".U,
    )
  )

  // Align load data: shift the loaded data based on the address offset and adjust based on the load size and sign
  val loadOffsetAligned = RegEnable(offsetAligned, io.lsuctrlIE.isLoad === LoadControl.EN)
  val loadDataAligned = {
    val shiftedData = io.edm.ld_rdata >> (loadOffsetAligned << 3).asUInt
    val slicedData = Mux1H(
      Seq(
        (io.lsuctrlME.lsSize === DataSize.Byte) -> shiftedData(7, 0),
        (io.lsuctrlME.lsSize === DataSize.HalfWord) -> shiftedData(15, 0),
        (io.lsuctrlME.lsSize === DataSize.Word) -> shiftedData,
      )
    )
    val signedData = Wire(UInt(xLen.W))
    signedData := Mux(io.lsuctrlME.isSigned === SignedControl.signed, slicedData.asSInt.asUInt, slicedData.asUInt)
    signedData
  }


  // Store buffer
  val sb = Module(new QueueWithAccessableEntry(new StoreBufferEntry, storeBufferEntries, flow = true))
  sb.io.enq.valid := io.lsuctrlIE.isStore === StoreControl.EN
  io.storeFull := !sb.io.enq.ready
  sb.io.enq.bits.addr := addrAligned
  sb.io.enq.bits.data := storeDataAligned
  sb.io.enq.bits.mask := storeDataMask


  // Request to DM
  io.edm.cmd := MuxCase(0.U, 
    Seq(
      (io.lsuctrlIE.isLoad === LoadControl.EN) -> M_XRD,
      ((sb.io.deq.valid) && (sb.io.deq.bits.mask === "b1111".U)) -> M_XWR,
      ((sb.io.deq.valid) && (sb.io.deq.bits.mask =/= "b1111".U)) -> M_XWR,
    )
  )

  io.edm.is_mmio := DontCare
  io.edm.st_mmio_reserv := DontCare

  // Load request issues in IE
  io.edm.ld_req := io.lsuctrlIE.isLoad === LoadControl.EN
  io.edm.ld_vaddr := addrAligned
  io.edm.ld_kill := DontCare
  io.edm.ld_mmio_kill := DontCare
  io.loadFull := !io.edm.ld_ack && io.edm.ld_req

  // Store request issues from store buffer
  io.edm.st_req := sb.io.deq.valid
  sb.io.deq.ready := io.edm.st_ack
  io.edm.st_paddr := sb.io.deq.bits.addr
  io.edm.st_wdata := sb.io.deq.bits.data
  io.edm.st_mask := sb.io.deq.bits.mask
  io.edm.st_mmio := DontCare

  // Response from DM: ME stage
  io.rddata := Mux(io.edm.ld_ack, loadDataAligned, 0.U)

  // Exception handling
  io.stXcpt := {
    val result = Wire(new HeartXcpt)
    (result.getElements zip io.edm.xcpt.getElements).foreach {
      case (a, b) => a.asInstanceOf[Bool] := b.asInstanceOf[Bool] && io.edm.st_ack
    }
    result
  }
  io.ldXcpt := {
    val result = Wire(new HeartXcpt)
    (result.getElements zip io.edm.xcpt.getElements).foreach {
      case (a, b) => a.asInstanceOf[Bool] := b.asInstanceOf[Bool] && io.edm.ld_ack
    }
    result
  }
}
