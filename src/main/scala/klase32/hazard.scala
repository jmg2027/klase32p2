package klase32

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import klase32.include.config._
import klase32.include.KLASE32AbstractClass._


class Hazards(implicit p: Parameters) extends CoreModule {
  val io = IO(new Bundle{
    val rs1Valid = Input(Bool())
    val rs2Valid = Input(Bool())
    val loadValidIE = Input(Bool())
    val loadValidME = Input(Bool())
    val loadDataValidME = Input(Bool())

    val rs1Addr = Input(UInt(regIdWidth.W)) // 19:15
    val rs2Addr = Input(UInt(regIdWidth.W)) // 24:20
    val rdAddrIE = Input(UInt(regIdWidth.W)) // 11:7, IE
    val rdAddrME = Input(UInt(regIdWidth.W)) // 11:7, ME

    val loadFull = Input(Bool())
    val loadAck = Input(Bool())

     val divAddr = Input(UInt(regIdWidth.W))
     val divBusy = Input(Bool())
     val divWrite = Input(Bool())

    val stall = Output(Bool())
    val bypassRS1RD = Output(Bool())
    val bypassRS2RD = Output(Bool())
    val ldNotWriteback = Output(Bool())
  }
  )

  // Bypass
  val loadRDUsingRS1 = io.loadDataValidME && io.rs1Valid && (io.rs1Addr === io.rdAddrME)
  val loadRDUsingRS2 = io.loadDataValidME && io.rs2Valid && (io.rs2Addr === io.rdAddrME)

  // Stall
  val loadRDUsingRD = io.loadValidME &&  (io.rdAddrIE === io.rdAddrME)

  // Bypass
  // RS1 === Load RD
  // RS2 === Load RD
  io.bypassRS1RD := false.B
  when(loadRDUsingRS1) {
    io.bypassRS1RD := true.B
  }

  io.bypassRS2RD := false.B
  when (loadRDUsingRS2) {
    io.bypassRS2RD := true.B
  }

  // Do not writeback
  io.ldNotWriteback := false.B
  // When using rd of load as next instruction's rd, ignore load
  when(loadRDUsingRD) {
    io.ldNotWriteback := true.B
  }

  // When load rd is used in rs1/rs2/rd of next instruction
//    val RAWHWraw = RegInit(false.B)
//  when(loadRDUsingRD || loadRDUsingRS1 || loadRDUsingRS2 || io.loadFull) {
//    RAWHWraw := true.B
//  }
//  val RAWHWraw = WireInit(false.B)
//  RAWHWraw := loadRDUsingRD || loadRDUsingRS1 || loadRDUsingRS2 || io.loadFull
  //    when(io.loadValidME && (io.rdAddrME === io.rs1Addr || io.rdAddrME === io.rs2Addr)) {
//      RAWHWraw := true.B
//    }
//    when(RAWHWraw && io.loadAck) {
//      RAWHWraw := false.B
//    }
//  val RAWHWraw = WireInit(false.B)
//  when(io.loadValidME && (io.rdAddrME === io.rs1Addr || io.rdAddrME === io.rs2Addr)) {
//    RAWHWraw := !io.loadAck
//  }
//  val read_after_divHWraw = Wire(Bool())
//   val write_after_divHWwaw = Wire(Bool())
  // val write_after_div_1HWwaw = Wire(Bool())
  // val div_busyHWnoDep = Wire(Bool())
  // val div_wpHWnoDep = Wire(Bool())

//  io.stall := RAWHWraw
    io.stall := DontCare
//  io.stall := RAWHWraw ||
//    read_after_divHWraw ||
//    write_after_divHWwaw
//
//
//  when(
//     (io.divBusy && (io.rs1Addr === io.divAddr)) ||
//     (io.divBusy && (io.rs2Addr === io.divAddr))
//   ) {
//     read_after_divHWraw := true.B
//   }.otherwise {
//     read_after_divHWraw := false.B
//   }
//
//   when(
//     (io.divBusy && (io.rdAddrIE === io.divAddr))
//   ) {
//     write_after_divHWwaw := true.B
//   }.otherwise {
//     write_after_divHWwaw := false.B
//   }

//  when(
//    io.divBusy &&
//  )

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
