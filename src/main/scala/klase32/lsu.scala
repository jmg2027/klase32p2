package klase32

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.util.BitPat.bitPatToUInt
import klase32.include.config._
import klase32.include.param.KLASE32ParamKey
import klase32.include.KLASE32AbstractClass._
import klase32.include.ControlSignal._
import klase32.include.{EdmIntf, HeartXcpt, LSUControl, QueueWithAccessableEntryWithValid, StoreBufferEntry}
import klase32.include.enums.DataSize
import klase32.include.util.LeadingOneDetector
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
    val storeEmpty = Output(Bool())
    val loadFull = Output(Bool())
    val loadEmpty = Output(Bool())

    val ldXcpt = Output(new HeartXcpt)
    val stXcpt = Output(new HeartXcpt)

    val stall = Input(Bool())
    val stallME = Input(Bool())

    val canLoadWriteback = Output(Bool())
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

  val storeBufferNotExists = storeBufferEntries != 0

  val loadMatchWithStoreBufferEntry = Reg(Valid(new StoreBufferEntry()))

  // Store buffer
  // In-order Store queue will monitor whether store is done by ack
  if (storeBufferNotExists) {
    val sb = Module(new QueueWithAccessableEntryWithValid(new StoreBufferEntry, storeBufferEntries, hasFlush = true))
    sb.io.enq.valid := (io.lsuctrlIE.isStore === StoreControl.EN) && !io.stall && io.edm.st_gnt
    sb.io.enq.bits.addr := io.edm.st_paddr
    sb.io.enq.bits.mask := io.edm.st_mask
    sb.io.enq.bits.data := io.edm.st_wdata
    io.storeFull := !sb.io.enq.ready
    io.storeEmpty := !sb.io.deq.valid

    // Store buffer flush when store exception occurs
    sb.flush := io.stXcpt.asUInt.orR

    // Check store buffer address for load
    //  printf(cf"[LSU] sb.enq: ${sb.io.enq}\n")
    val loadMatchStoreVec = VecInit(Seq.tabulate(storeBufferEntries) { i => false.B })
    val loadMatchData = WireInit(0.U(k.dataWidth.W))

    // Compare load and store address
    for (i <- 0 until storeBufferEntries) {
      loadMatchStoreVec(i) := (sb.entry(i).addr === io.addr) && (io.lsuctrlIE.isLoad === LoadControl.EN)
      loadMatchData := PriorityMux(Seq(
        loadMatchStoreVec(storeBufferEntries - i - 1) -> sb.entry(i).data
      ))
    }

    val lod = Module(new LeadingOneDetector(storeBufferEntries))
    lod.io.in := loadMatchStoreVec.asUInt
    val msbPosition = lod.io.out

    loadMatchWithStoreBufferEntry.valid := loadMatchStoreVec.reduce({
      _ || _
    })
    loadMatchWithStoreBufferEntry.bits.data := sb.entry(msbPosition).data

    io.edm.st_req := sb.io.enq.fire
    sb.io.deq.ready := io.edm.st_ack
    // Store buffer
  } else {
    val StoreRequestOnTheFly = RegInit(false.B)
    when (io.edm.st_req && !io.edm.st_ack) {
      StoreRequestOnTheFly := true.B
    }
    when(StoreRequestOnTheFly) {
      when(io.edm.st_ack) {
        StoreRequestOnTheFly := false.B
      }
    }

    io.storeFull := StoreRequestOnTheFly && !io.edm.st_ack
    io.storeEmpty := !StoreRequestOnTheFly

    loadMatchWithStoreBufferEntry.valid := false.B
    loadMatchWithStoreBufferEntry.bits := 0.U.asTypeOf(loadMatchWithStoreBufferEntry.bits)

    io.edm.st_req := (io.lsuctrlIE.isStore === StoreControl.EN) && !io.stall && !io.storeFull
  }
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

  // Request to DM
  io.edm.cmd := MuxCase(0.U,
    Seq(
      ((io.lsuctrlIE.isLoad === LoadControl.EN)) -> M_XRD,
      ((io.lsuctrlIE.isStore === StoreControl.EN) && (io.edm.st_mask === "b1111".U)) -> M_XWR,
      ((io.lsuctrlIE.isStore === StoreControl.EN) && (io.edm.st_mask =/= "b1111".U)) -> M_PWR,
    )
  )

  io.edm.is_mmio := DontCare
  io.edm.st_mmio_reserv := DontCare

  // Load request issues in IE
  // FIXME: store buffer
//  io.edm.ld_req := (io.lsuctrlIE.isLoad === LoadControl.EN) && !io.stall && !loadMatchStore
  io.edm.ld_req := (io.lsuctrlIE.isLoad === LoadControl.EN) && !io.stall && io.edm.ld_gnt
  io.edm.ld_vaddr := addrAligned
  io.edm.ld_kill := DontCare
  io.edm.ld_mmio_kill := DontCare

  val loadRequestOnthefly = RegInit(false.B)

//  io.loadFull := loadRequestOnthefly && io.lsuctrlIE.isLoad === LoadControl.EN // When multiple load requires
  // No multiple load
  io.loadFull := loadRequestOnthefly && !io.edm.ld_ack
  when (!loadRequestOnthefly) {
    when(io.edm.ld_req && !io.edm.ld_ack) {
      loadRequestOnthefly := true.B
    }
  }.otherwise {
//    when (io.edm.ld_ack || io.ldKill) {
    when (!io.edm.ld_req && io.edm.ld_ack) {
      loadRequestOnthefly := false.B
    }
  }
  // 1 load can be on the fly, so empty means no request is on the fly
  io.loadEmpty := !loadRequestOnthefly

  // Store request issues from store buffer
  // FIXME: Why do we need to stall signals for enq/deq?
  // FIXME: Within TCM address bound, store should not be stalled
  // FIXME: TCM address comes from io port
//  io.edm.st_req := sb.io.deq.start && !io.stall
  // Request is sent when store buffer can accept store request
  // Store buffer will dequeue when ack arrives
//  if (storeBufferNotExists) {
//    io.edm.st_req := sb.io.enq.fire
//    sb.io.deq.ready := io.edm.st_ack
//  }
  io.edm.st_paddr := addrAligned
  io.edm.st_wdata := storeDataAligned
  io.edm.st_mask := storeDataMask
  io.edm.st_mmio := DontCare
//  printf(cf"[LSU]io.edm: ${io.edm}\n")

  // Response from DM: ME stage
  //  val loadData = RegInit(0.U(k.dataWidth.W))
  //  when (io.edm.ld_ack && io.stallME) {
  //    io.rddata := 0.U
  //    loadData := loadDataAligned
  //  }.elsewhen (!io.stallME) {
  //    io.rddata := loadData
  //  }.otherwise {
  //    io.rddata := loadDataAligned
  //  }

  // When load address matches with address of store buffer entry
  // No load response occurs, so loadDataAligned does not switch, means prevent energy consumption
  // There is no out-of-order loads, so ld_ack and loadMatchStore is perpendicular
  // When stall, load data should be preserved. Or else load data will directly write back into register file
  val loadData = WireInit(0.U(k.dataWidth.W))
  val skidLoadData = Reg(Valid(UInt(k.dataWidth.W)))

  io.canLoadWriteback := io.edm.ld_ack || loadMatchWithStoreBufferEntry.valid
  when(io.edm.ld_ack) {
    loadData := loadDataAligned
  }.elsewhen(loadMatchWithStoreBufferEntry.valid) {
    loadData := loadMatchWithStoreBufferEntry.bits.data
  }

  when(io.stallME && !skidLoadData.valid && io.canLoadWriteback) {
    skidLoadData.valid := true.B
    skidLoadData.bits := loadData
    io.rddata := loadData
  }.elsewhen (!io.stallME && skidLoadData.valid && io.canLoadWriteback) {
    // In case skid buffer is valid and ack arrives
    // Write back skid buffer data and refill it by ack data
    skidLoadData.valid := true.B
    skidLoadData.bits := skidLoadData.bits
    io.rddata := skidLoadData.bits
  }.elsewhen(!io.stallME && skidLoadData.valid && !io.canLoadWriteback) {
    skidLoadData.valid := false.B
    skidLoadData.bits := skidLoadData.bits
    io.rddata := skidLoadData.bits
  }.otherwise {
    skidLoadData.valid := false.B
    skidLoadData.bits := skidLoadData.bits
    io.rddata := loadData
  }

  // Exception handling
  // Assuming imprecise store exception for performance improvement
  // However if we use page fault, it should be handled as preicise exception
  // https://www.cs.yale.edu/homes/abhishek/gupta-isca23.pdf
  // Can we at least save pc of exception? This may be helpful instead of saving all register files
  // We should deal with multiple outstanding loads/stores and mmio requests... later
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
