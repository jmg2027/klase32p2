package klase32.functionalunit

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.util.BitPat.bitPatToUInt
import klase32.include.config._
import klase32.include.param.KLASE32ParamKey
import klase32.include.KLASE32AbstractClass._
import klase32.include.ControlSignal.{FenceEnableIE, _}
import klase32.include.{EdmIntf, HeartXcpt, LSUControl, QueueWithAccessableEntryWithValid, StoreBufferEntry}
import klase32.include.enums.DataSize
import klase32.include.util.LeadingOneDetector
import freechips.rocketchip.rocket.constants.MemoryOpConstants
import freechips.rocketchip.rocket.Causes
import klase32.common.FunctionUnitIO

class LSUReq(implicit p: Parameters) extends CoreBundle {
  private val k = p(KLASE32ParamKey)

  val ctrl = new LSUControl
  val addr = UInt(k.vaddrBits.W)
  val wrdata = UInt(k.dataWidth.W)
  val fence = FenceEnableIE() // How to deal with fence?
  val rd = UInt(regIdWidth.W)
}

class LSUResp(implicit p: Parameters) extends CoreBundle {
  private val k = p(KLASE32ParamKey)

  val rddata = UInt(k.dataWidth.W)
  val loadAck = Bool()
  val xcpt = new HeartXcpt
  val rd = UInt(regIdWidth.W)
}

class LSUCoreIn(implicit p: Parameters) extends CoreBundle {
  private val k = p(KLASE32ParamKey)

  val ctrl = new LSUControl
  val addr = UInt(k.vaddrBits.W)
  val wrdata = UInt(k.dataWidth.W)
  val fence = Bool() // How to deal with fence?
  val rd = UInt(regIdWidth.W)
}

class LSUCoreOut(implicit p: Parameters) extends CoreBundle {
  private val k = p(KLASE32ParamKey)

  val rddata = UInt(k.dataWidth.W)
  val loadAck = Bool()
  val xcpt = new HeartXcpt
  val rd = UInt(regIdWidth.W)
  val storeFull, loadFull = Bool()
  val storeEmpty, loadEmpty = Bool()
}

class LSU(implicit p: Parameters) extends CoreModule
with FunctionUnitIO[LSUReq, LSUResp] {
  val req = IO(Flipped(Decoupled(new LSUReq)))
  val resp = IO(Decoupled(new LSUResp))

  val io = IO(new Bundle {
    val edm = new EdmIntf()
  })

  val core = Module(new LSUCore)

  private val isLoad = req.bits.ctrl.isLoad === LoadControl.EN
  private val isStore = req.bits.ctrl.isStore === StoreControl.EN
  private val isFence = req.bits.fence === FenceEnableIE.EN
  req.ready := !core.io.out.storeFull &&
    (!isLoad || !core.io.out.loadFull) &&
    (!isFencne || core.io.out.storeEmpty && core.io.out.loadEmpty)

  // How to deal with fire policy?
  when(req.fire) {
    core.io.in.ctrl := req.bits.ctrl
    core.io.in.addr := req.bits.addr
    core.io.in.wrdata := req.bits.wrdata
    core.io.in.rd := req.bits.rd
  }

  resp.valid := core.io.out.loadAck
  resp.bits.rddata := core.io.out.rddata
  resp.bits.rd := core.io.out.rd
  resp.bits.loadAck := core.io.out.loadAck
  resp.bits.xcpt := core.io.out.xcpt

  resp.ready := true.B
}

class LSUCore(implicit p: Parameters) extends CoreModule with MemoryOpConstants {
  val k = p(KLASE32ParamKey)

  val io = IO(new Bundle {
    val in = Input(new LSUCoreIn())
    val out = Output(new LSUCoreOut())

    val edm = new EdmIntf()
  })

  // Align address
  val addrAligned = Cat(io.in.addr(k.vaddrBits - 1, log2Ceil(addressAlignByte)), 0.U(log2Ceil(addressAlignByte).W))
  val offsetAligned = io.in.addr(log2Ceil(addressAlignByte) - 1, 0)

  // Align store data: shift the write data based on the address offset and create a mask for the store operatreq.bitsn
  val storeDataAligned = (io.in.wrdata << (offsetAligned << 3.U).asUInt).asUInt
  val storeDataMask = Mux1H(
    Seq(
      (io.in.ctrl.lsSize === DataSize.Byte) -> ("b0001".U << offsetAligned).asUInt,
      (io.in.ctrl.lsSize === DataSize.HalfWord) -> ("b0011".U << offsetAligned).asUInt,
      (io.in.ctrl.lsSize === DataSize.Word) -> "b1111".U,
    )
  )

  val storeBufferNotExists = storeBufferEntries != 0

  val loadMatchWithStoreBufferEntry = Reg(Valid(new StoreBufferEntry()))

  // Store buffer
  // In-order Store queue will monitor whether store is done by ack
  if (storeBufferNotExists) {
    val sb = Module(new QueueWithAccessableEntryWithValid(new StoreBufferEntry, storeBufferEntries, hasFlush = true))
    sb.io.enq.valid := (io.in.ctrl.isStore === StoreControl.EN) && io.edm.st_gnt
    sb.io.enq.bits.addr := io.edm.st_paddr
    sb.io.enq.bits.mask := io.edm.st_mask
    sb.io.enq.bits.data := io.edm.st_wdata
    io.out.storeFull := !sb.io.enq.ready
    io.out.storeEmpty := !sb.io.deq.valid

    // Store buffer flush when store exception occurs
    sb.flush := io.out.xcpt.asUInt.orR

    // Check store buffer address for load
    //  printf(cf"[LSU] sb.enq: ${sb.io.enq}\n")
    val loadMatchStoreVec = VecInit(Seq.tabulate(storeBufferEntries) { i => false.B })
    val loadMatchData = WireInit(0.U(k.dataWidth.W))

    // Compare load and store address
    for (i <- 0 until storeBufferEntries) {
      loadMatchStoreVec(i) := (sb.entry(i).addr === io.in.addr) && (io.in.ctrl.isLoad === LoadControl.EN)
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

    io.out.storeFull := StoreRequestOnTheFly && !io.edm.st_ack
    io.out.storeEmpty := !StoreRequestOnTheFly

    loadMatchWithStoreBufferEntry.valid := false.B
    loadMatchWithStoreBufferEntry.bits := 0.U.asTypeOf(loadMatchWithStoreBufferEntry.bits)

    io.edm.st_req := (io.in.ctrl.isStore === StoreControl.EN) && !io.out.storeFull
  }
  // Align load data: shift the loaded data based on the address offset and adjust based on the load size and sign
  val meCtrl = RegEnable(io.in.ctrl, (io.in.ctrl.isLoad === LoadControl.EN))
  val loadOffsetAligned = RegEnable(offsetAligned, (io.in.ctrl.isLoad === LoadControl.EN))
  val loadDataAligned = {
    val shiftedData = (io.edm.ld_rdata >> (loadOffsetAligned << 3.U).asUInt).asUInt
    val loadData = Mux1H(
      Seq(
        ((meCtrl.lsSize === DataSize.Byte) && (meCtrl.isSigned === SignedControl.unsigned)) -> shiftedData(7, 0),
        ((meCtrl.lsSize === DataSize.Byte) && (meCtrl.isSigned === SignedControl.signed)) -> Cat(Fill(k.dataWidth - 8, shiftedData(7)), shiftedData(7, 0)),
        ((meCtrl.lsSize === DataSize.HalfWord) && (meCtrl.isSigned === SignedControl.unsigned)) -> shiftedData(15, 0),
        ((meCtrl.lsSize === DataSize.HalfWord) && (meCtrl.isSigned === SignedControl.signed)) -> Cat(Fill(k.dataWidth - 16, shiftedData(15)), shiftedData(15, 0)),
        (meCtrl.lsSize === DataSize.Word) -> shiftedData,
      )
    )
    loadData
  }

  // Request to DM
  io.edm.cmd := MuxCase(0.U,
    Seq(
      ((io.in.ctrl.isLoad === LoadControl.EN)) -> M_XRD,
      ((io.in.ctrl.isStore === StoreControl.EN) && (io.edm.st_mask === "b1111".U)) -> M_XWR,
      ((io.in.ctrl.isStore === StoreControl.EN) && (io.edm.st_mask =/= "b1111".U)) -> M_PWR,
    )
  )

  io.edm.is_mmio := DontCare
  io.edm.st_mmio_reserv := DontCare

  // Load request issues in IE
  // FIXME: store buffer
//  io.edm.ld_req := (io.lsuctrlIE.isLoad === LoadControl.EN) && !io.stall && !loadMatchStore
  io.edm.ld_req := (io.in.ctrl.lsuctrlIE.isLoad === LoadControl.EN) && io.edm.ld_gnt
  io.edm.ld_vaddr := addrAligned
  io.edm.ld_kill := DontCare
  io.edm.ld_mmio_kill := DontCare

  val loadRequestOnthefly = RegInit(false.B)

  // No multiple load
  io.out.loadFull := loadRequestOnthefly && !io.edm.ld_ack
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
  io.out.loadEmpty := !loadRequestOnthefly

  // Store request issues from store buffer
  // FIXME: Why do we need to stall signals for enq/deq?
  // FIXME: Within TCM address bound, store should not be stalled
  // FIXME: TCM address comes from io port
  // Request is sent when store buffer can accept store request
  // Store buffer will dequeue when ack arrives
  io.edm.st_paddr := addrAligned
  io.edm.st_wdata := storeDataAligned
  io.edm.st_mask := storeDataMask
  io.edm.st_mmio := DontCare

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

  when(!skidLoadData.valid && io.canLoadWriteback) {
    skidLoadData.valid := true.B
    skidLoadData.bits := loadData
    io.out.rddata := loadData
  }.elsewhen (skidLoadData.valid && io.canLoadWriteback) {
    // In case skid buffer is valid and ack arrives
    // Write back skid buffer data and refill it by ack data
    skidLoadData.valid := true.B
    skidLoadData.bits := skidLoadData.bits
    io.out.rddata := skidLoadData.bits
  }.elsewhen(skidLoadData.valid && !io.canLoadWriteback) {
    skidLoadData.valid := false.B
    skidLoadData.bits := skidLoadData.bits
    io.out.rddata := skidLoadData.bits
  }.otherwise {
    skidLoadData.valid := false.B
    skidLoadData.bits := skidLoadData.bits
    io.out.rddata := loadData
  }

  // Exception handling
  io.out.xcpt = io.edm.xcpt
}
