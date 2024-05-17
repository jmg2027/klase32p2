package klase32

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import klase32.config._
import klase32.param.KLASE32ParamKey
import _root_.klase32.HazardME.RD


class Hazards(implicit p: Parameters) extends CoreModule {
  val k = p(KLASE32ParamKey)

  val io = IO(new Bundle{
    val rs1Valid = Input(Bool())
    val rs2Valid = Input(Bool())
    val loadValid = Input(Bool())

    val rs1Addr = Input(UInt(regIdWidth.W)) // 19:15
    val rs2Addr = Input(UInt(regIdWidth.W)) // 24:20
    val rdAddr = Input(UInt(regIdWidth.W)) // 11:7, ME

    // val divAddr = Input(UInt(regIdWidth.W))
    // val divBusy = Input(Bool())
    // val divWnc = Input(Bool())

    val stall = Output(Bool())
    val bypassRS1RD = Output(Bool())
    val bypassRS2RD = Output(Bool())
  }
  )

  val RAWHWraw = false.B
  // val read_after_divHWraw = Wire(Bool())
  // val write_after_divHWwaw = Wire(Bool())
  // val write_after_div_1HWwaw = Wire(Bool())
  // val div_busyHWnoDep = Wire(Bool())
  // val div_wpHWnoDep = Wire(Bool())

  // io.stall := RAWHWraw || read_after_divHWraw || write_after_divHWwaw || write_after_div_1HWwaw || div_busyHWnoDep || div_wpHWnoDep
  io.stall := RAWHWraw

  // RS1 === Load RD
  // RS2 === Load RD
  io.bypassRS1RD := false.B
  when(io.loadValid && io.rs1Valid && (io.rs1Addr === io.rdAddr)) {
    io.bypassRS1RD := true.B
  }

  io.bypassRS2RD := false.B
  when (io.loadValid && io.rs2Valid && (io.rs2Addr === io.rdAddr)) {
    io.bypassRS2RD := true.B
  }

  // Bypass


  // when(
  //   (io.divBusy && io.ctrlIE(0) && (io.rs1Addr === io.divAddr)) ||
  //   (io.divBusy && io.ctrlIE(8) && (io.rsdAddr === io.divAddr)) ||
  //   (io.divBusy && io.ctrlIE(4) && (io.rs2Addr === io.divAddr)) ||
  //   (io.divBusy && io.ctrlIE(9) && (0.U === io.divAddr))
  //   (io.divBusy && io.ctrlIE(3) && (2.U === io.divAddr))
  // ) {
  //   read_after_divHWraw := 1.U
  // }.otherwise {
  //   read_after_divHWraw := 0.U
  // }

  // when(
  //   (io.divBusy && io.ctrlIE(10) && (io.rsdAddr === io.divAddr)) ||
  //   (io.divBusy && io.ctrlIE(13) && (1.U === io.divAddr)) ||
  //   (io.divBusy && io.ctrlIE(14) && (2.U === io.divAddr))
  // ) {
  //   write_after_divHWwaw := 1.U
  // }.otherwise {
  //   write_after_divHWwaw := 0.U
  // }

  // when(
  //   (io.divBusy && io.ctrlIE(15) && (io.rsdAddr === io.divAddr)) ||
  // ) {
  //   write_after_div_1HWwaw := 1.U
  // }.otherwise {
  //   write_after_div_1HWwaw := 0.U
  // }

  // when(
  //   (io.divBusy && io.ctrlIE(17))
  // ) {
  //   div_busyHWnoDep := 1.U
  // }.otherwise {
  //   div_busyHWnoDep := 0.U
  // }

  // when(
  //   (io.divWnc && io.ctrlIE(18))
  // ) {
  //   div_wpHWnoDep := 1.U
  // }.otherwise {
  //   div_wpHWnoDep := 0.U
  // }
}
