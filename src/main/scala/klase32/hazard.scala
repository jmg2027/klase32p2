package klase32

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import klase32.config._
import klase32.param.KlasE32ParamKey


class Hazards(implicit p: Parameters) extends CoreModule {
  val k = p(KlasE32ParamKey)

  val io = IO(new Bundle{
    val hzdCtrl = Input(HazardIE())
    val hzdLoadCtrl = Input(HazardME())

    val rsdAddr = Input(UInt(regIdWidth.W)) // 11:7
    val rs1Addr = Input(UInt(regIdWidth.W)) // 19:15
    val rs2Addr = Input(UInt(regIdWidth.W)) // 24:20
    val rdAddr = Input(UInt(regIdWidth.W)) // 11:7, ME

    val Crs2Addr_4_2 = Input(UInt((regIdWidth-2).W)) // 4:2
    val Crs2Addr = Input(UInt(regIdWidth.W)) // 6:2
    val Crs1Addr = Input(UInt((regIdWidth-2).W)) // 9:7
    val CrdAddr = Input(UInt((regIdWidth-2).W)) // 4:2, ME

    val divAddr = Input(UInt(regIdWidth.W))
    val divBusy = Input(Bool())
    val divWnc = Input(Bool())

    val stall = Output(Bool())
    }
  )

  val RAWHWraw = Wire(Bool())
  val RAW_1HWraw = Wire(Bool())
  val read_after_divHWraw = Wire(Bool())
  val write_after_divHWwaw = Wire(Bool())
  val write_after_div_1HWwaw = Wire(Bool())
  val div_busyHWnoDep = Wire(Bool())
  val div_wpHWnoDep = Wire(Bool())

  io.stall := RAWHWraw || RAW_1HWraw || read_after_divHWraw || write_after_divHWwaw || write_after_div_1HWwaw || div_busyHWnoDep || div_wpHWnoDep

  when(
    (io.ctrlME(0) && io.ctrlIE(0) && (io.rs1Addr === io.rdAddr)) ||
    (io.ctrlME(0) && io.ctrlIE(1) && (io.rsdAddr === io.rdAddr)) ||
    (io.ctrlME(0) && io.ctrlIE(2) && (io.Crs1Addr === io.rdAddr)) ||
    (io.ctrlME(0) && io.ctrlIE(3) && (2.U === io.rdAddr)) ||
    (io.ctrlME(1) && io.ctrlIE(0) && (io.CrdAddr === io.rs1Addr)) ||
    (io.ctrlME(1) && io.ctrlIE(1) && (io.CrdAddr === io.rsdAddr)) ||
    (io.ctrlME(1) && io.ctrlIE(2) && (io.Crs1Addr === io.CrdAddr))
  ) {
    RAWHWraw := 1.U
  }.otherwise {
    RAWHWraw := 0.U
  }

  when(
    (io.ctrlME(0) && io.ctrlIE(4) && (io.rs2Addr === io.rdAddr)) ||
    (io.ctrlME(0) && io.ctrlIE(5) && (io.Crs2Addr === io.rdAddr)) ||
    (io.ctrlME(0) && io.ctrlIE(6) && (io.rsdAddr === io.rdAddr)) ||
    (io.ctrlME(0) && io.ctrlIE(7) && (io.Crs2Addr_4_2 === io.rdAddr)) ||
    (io.ctrlME(1) && io.ctrlIE(4) && (io.CrdAddr === io.rs2Addr)) ||
    (io.ctrlME(1) && io.ctrlIE(5) && (io.CrdAddr === io.Crs2Addr)) ||
    (io.ctrlME(1) && io.ctrlIE(6) && (io.CrdAddr === io.rdAddr))
    (io.ctrlME(1) && io.ctrlIE(7) && (io.Crs2Addr_4_2 === io.CrdAddr))
  ) {
    RAW_1HWraw := 1.U
  }.otherwise {
    RAW_1HWraw := 0.U
  }

  when(
    (io.divBusy && io.ctrlIE(0) && (io.rs1Addr === io.divAddr)) ||
    (io.divBusy && io.ctrlIE(8) && (io.rsdAddr === io.divAddr)) ||
    (io.divBusy && io.ctrlIE(4) && (io.rs2Addr === io.divAddr)) ||
    (io.divBusy && io.ctrlIE(5) && (io.Crs2Addr === io.divAddr)) ||
    (io.divBusy && io.ctrlIE(2) && (io.Crs1Addr === io.divAddr)) ||
    (io.divBusy && io.ctrlIE(7) && (io.Crs2Addr_4_2 === io.divAddr)) ||
    (io.divBusy && io.ctrlIE(9) && (0.U === io.divAddr))
    (io.divBusy && io.ctrlIE(3) && (2.U === io.divAddr))
  ) {
    read_after_divHWraw := 1.U
  }.otherwise {
    read_after_divHWraw := 0.U
  }

  when(
    (io.divBusy && io.ctrlIE(10) && (io.rsdAddr === io.divAddr)) ||
    (io.divBusy && io.ctrlIE(11) && (io.Crs2Addr_4_2 === io.divAddr)) ||
    (io.divBusy && io.ctrlIE(12) && (io.Crs1Addr === io.divAddr)) ||
    (io.divBusy && io.ctrlIE(13) && (1.U === io.divAddr)) ||
    (io.divBusy && io.ctrlIE(14) && (2.U === io.divAddr))
  ) {
    write_after_divHWwaw := 1.U
  }.otherwise {
    write_after_divHWwaw := 0.U
  }

  when(
    (io.divBusy && io.ctrlIE(15) && (io.rsdAddr === io.divAddr)) ||
    (io.divBusy && io.ctrlIE(16) && (io.Crs2Addr_4_2 === io.divAddr))
  ) {
    write_after_div_1HWwaw := 1.U
  }.otherwise {
    write_after_div_1HWwaw := 0.U
  }

  when(
    (io.divBusy && io.ctrlIE(17))
  ) {
    div_busyHWnoDep := 1.U
  }.otherwise {
    div_busyHWnoDep := 0.U
  }

  when(
    (io.divWnc && io.ctrlIE(18))
  ) {
    div_wpHWnoDep := 1.U
  }.otherwise {
    div_wpHWnoDep := 0.U
  }
}
