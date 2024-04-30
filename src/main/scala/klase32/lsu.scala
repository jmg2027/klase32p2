package klase32

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.util.BitPat.bitPatToUInt
import klase32.config._
import klase32.param.KlasE32ParamKey
import snitch.enums.DataSize
import freechips.rocketchip.rocket.constants.MemoryOpConstants


class LSU(implicit p: Parameters) extends CoreModule with MemoryOpConstants {
  val k = p(KlasE32ParamKey)

  val io = IO(new Bundle{
    val lsuctrlIE = Input(new LSUControl)
    val lsuctrlME = Input(new LSUControl)

    val edm = new EdmIntf()

    val addr = Input(UInt(k.addrWidth.W))
    val rddata = Output(UInt(k.dataWidth.W))
    val wrdata = Input(UInt(k.dataWidth.W))

    val rs1addr = Input(UInt(regIdWidth.W))
    val rs2addr = Input(UInt(regIdWidth.W))
  }
  )

  // LSQ
  // val lsq = Module(new Queue(new LoadStoreQueueIntf, loadstorequeueEntries, hasFlush = true))
  val lsq = Vec(loadstorequeueEntries, Reg(new LoadStoreQueueIntf()))
  val checkLoadDependency = lsq.map(x => x.isLoad && ((x.rdaddr === io.rs1addr) && (x.rdaddr === io.rs2addr)))

  // lsq stall: rd is used in previous pipeline and ack not yet reached
  // in-order lsq, no address check
  // check dependency only
  // if no dependency, no stall and load/store continues to be enq
  // lsq entry: isload, rd
  // stalls when dependency occurs (IE/RSX, ME/RD)
  // enq cond: !full
  // fence -> stall pipeline until all lsq entries are committed -> empty
  // load/store request should be issued in sequence
  // enq and memory request


  // Request to DM
  io.edm.cmd := Mux1H(Seq(
    (io.lsuctrlIE.isLoad === LoadControl.EN) -> M_XRD,
    ((io.lsuctrlIE.isStore === StoreControl.EN) &&
      (io.lsuctrlIE.lsSize === DataSize.Word)) -> M_XWR,
    ((io.lsuctrlIE.isStore === StoreControl.EN) &&
      (io.lsuctrlIE.lsSize =/= DataSize.Word)) -> M_PWR,
  )
  )
  io.edm.is_mmio := DontCare
  io.edm.st_mmio_reserv := DontCare
  // io.edm.xcpt

  io.edm.ld_req := io.lsuctrlIE.isLoad === LoadControl.EN
  io.edm.ld_vaddr := io.addr
  io.edm.ld_kill := DontCare
  io.edm.ld_mmio_kill := DontCare

  io.edm.st_req := io.lsuctrlIE.isStore === StoreControl.EN
  io.edm.st_paddr := io.addr
  io.edm.st_wdata := io.wrdata
  io.edm.st_mask := Mux1H(
    Seq(
      (io.lsuctrlIE.lsSize === DataSize.Byte) -> "b0001".U,
      (io.lsuctrlIE.lsSize === DataSize.HalfWord) -> "b0011".U,
      (io.lsuctrlIE.lsSize === DataSize.Word) -> "b1111".U,
    )
  )
  io.edm.st_mmio := DontCare
  // ld_ack, st_ack

  // Response from DM: ME stage
  io.rddata := Mux1H(
    Seq(
      ((io.lsuctrlME.lsSize === DataSize.Byte) &&
        (io.lsuctrlME.isSigned === SignedControl.signed)) -> Cat(Fill(k.dataWidth - 7, 0.U), io.edm.ld_rdata(7, 0)),
      ((io.lsuctrlME.lsSize === DataSize.Byte) &&
        (io.lsuctrlME.isSigned === SignedControl.unsigned)) -> io.edm.ld_rdata(7, 0).zext.asUInt,
      ((io.lsuctrlME.lsSize === DataSize.HalfWord) &&
        (io.lsuctrlME.isSigned === SignedControl.signed)) -> Cat(Fill(k.dataWidth - 15, 0.U), io.edm.ld_rdata(15, 0)),
      ((io.lsuctrlME.lsSize === DataSize.HalfWord) &&
        (io.lsuctrlME.isSigned === SignedControl.unsigned)) -> io.edm.ld_rdata(15, 0).zext.asUInt,
      (io.lsuctrlME.lsSize === DataSize.Word) -> io.edm.ld_rdata,
    )
  )
}
