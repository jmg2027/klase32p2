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

    val addr = Input(UInt(k.vaddrBits.W))
    val rddata = Output(UInt(k.dataWidth.W))
    val wrdata = Input(UInt(k.dataWidth.W))

    val storeFull = Output(Bool())
    val loadFull = Output(Bool())

    val ldXcpt = Output(new HeartXcpt)
    val stXcpt = Output(new HeartXcpt)

    val stall = Input(Bool())
    val stallME = Input(Bool())

    val ldKill = Input(Bool())
  }
  )
  // Align address
  val addrAligned = Cat(io.addr(k.vaddrBits - 1, log2Ceil(addressAlignByte)), 0.U(log2Ceil(addressAlignByte).W))
  val offsetAligned = io.addr(log2Ceil(addressAlignByte) - 1, 0)

  // Align store data: shift the write data based on the address offset and create a mask for the store operation
  val storeDataAligned = (io.wrdata << (offsetAligned << 3.U).asUInt).asUInt
  val storeDataMask = Mux1H(
    Seq(
      (io.lsuctrlIE.lsSize === DataSize.Byte) -> ("b0001".U << offsetAligned).asUInt,
      (io.lsuctrlIE.lsSize === DataSize.HalfWord) -> ("b0011".U << offsetAligned).asUInt,
      (io.lsuctrlIE.lsSize === DataSize.Word) -> "b1111".U,
    )
  )

  // Align load data: shift the loaded data based on the address offset and adjust based on the load size and sign
  val loadOffsetAligned = RegEnable(offsetAligned, (io.lsuctrlIE.isLoad === LoadControl.EN && !io.stall))
  val loadDataAligned = {
    val shiftedData = (io.edm.ld_rdata >> (loadOffsetAligned << 3.U).asUInt).asUInt
//    val slicedData = Mux1H(
    val loadData = Mux1H(
      Seq(
        ((io.lsuctrlME.lsSize === DataSize.Byte) && (io.lsuctrlME.isSigned === SignedControl.unsigned)) -> shiftedData(7, 0),
        ((io.lsuctrlME.lsSize === DataSize.Byte) && (io.lsuctrlME.isSigned === SignedControl.signed)) -> Cat(Fill(k.dataWidth - 8, shiftedData(7)), shiftedData(7, 0)),
        ((io.lsuctrlME.lsSize === DataSize.HalfWord) && (io.lsuctrlME.isSigned === SignedControl.unsigned)) -> shiftedData(15, 0),
        ((io.lsuctrlME.lsSize === DataSize.HalfWord) && (io.lsuctrlME.isSigned === SignedControl.signed)) -> Cat(Fill(k.dataWidth - 16, shiftedData(15)), shiftedData(15, 0)),
        (io.lsuctrlME.lsSize === DataSize.Word) -> shiftedData,
      )
    )
//    printf(cf"loadOffset: $loadOffsetAligned\n")
//    printf(cf"shiftedData: $shiftedData%x\n")
    loadData
  }
//  printf(cf"loaddata: $loadDataAligned%x\n")


  // Store buffer
  // In-order Store queue will monitor whether store is done by ack
  val sb = Module(new Queue(new StoreBufferEntry, storeBufferEntries))
  sb.io.enq.bits.valid := true.B
  sb.io.enq.valid := (io.lsuctrlIE.isStore === StoreControl.EN) && !io.stall
  io.storeFull := !sb.io.enq.ready
//  printf(cf"[LSU] sb.enq: ${sb.io.enq}\n")


  // Request to DM
  io.edm.cmd := MuxCase(0.U,
    Seq(
      (io.edm.ld_req) -> M_XRD,
      ((io.edm.st_req) && (io.edm.st_mask === "b1111".U)) -> M_XWR,
      ((io.edm.st_req) && (io.edm.st_mask =/= "b1111".U)) -> M_XWR,
    )
  )

  io.edm.is_mmio := DontCare
  io.edm.st_mmio_reserv := DontCare

  // Load request issues in IE
  io.edm.ld_req := (io.lsuctrlIE.isLoad === LoadControl.EN) && !io.stall
  io.edm.ld_vaddr := addrAligned
  io.edm.ld_kill := io.ldKill
  io.edm.ld_mmio_kill := DontCare

  val loadRequestOnthefly = RegInit(false.B)

  //  io.loadFull := !io.edm.ld_ack && io.edm.ld_req
  io.loadFull := loadRequestOnthefly
  when (io.edm.ld_req && !io.edm.ld_ack) {
    loadRequestOnthefly := true.B
  }
  when (loadRequestOnthefly) {
    when (io.edm.ld_ack || io.ldKill) {
      loadRequestOnthefly := false.B
    }
  }

  // FIXME: to prevent glitch..?
//  val stAckCapture = RegNext(io.edm.st_ack, false.B)
//  val ldAckCapture = RegNext(io.edm.ld_ack, false.B)

  // Store request issues from store buffer
  // FIXME: Why do we need to stall signals for enq/deq?
  // FIXME: Within TCM address bound, store should not be stalled
  // FIXME: TCM address comes from io port
//  io.edm.st_req := sb.io.deq.valid && !io.stall
  // Request is sent when store buffer can accept store request
  // Store buffer will dequeue when ack arrives
  io.edm.st_req := sb.io.enq.fire
  sb.io.deq.ready := io.edm.st_ack
//  sb.io.deq.ready := stAckCapture
  io.edm.st_paddr := addrAligned
  io.edm.st_wdata := storeDataAligned
  io.edm.st_mask := storeDataMask
  io.edm.st_mmio := DontCare
  printf(cf"[LSU]io.edm: ${io.edm}\n")

  // Response from DM: ME stage
  val loadData = RegInit(0.U(k.dataWidth.W))
  when (io.edm.ld_ack && io.stallME) {
    io.rddata := 0.U
    loadData := loadDataAligned
  }.elsewhen (!io.stallME) {
    io.rddata := loadData
  }.otherwise {
    io.rddata := loadDataAligned
  }

  // Exception handling
  io.stXcpt := {
    val result = Wire(new HeartXcpt)
    (result.getElements zip io.edm.xcpt.getElements).foreach {
      case (a, b) => a.asInstanceOf[Bool] := b.asInstanceOf[Bool] && io.edm.st_ack
//      case (a, b) => a.asInstanceOf[Bool] := b.asInstanceOf[Bool] && stAckCapture
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
