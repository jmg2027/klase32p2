package klase32.common

import chisel3._
import chisel3.util._

trait FunctionUnitIO[ReqT <: Data, RespT <: Data] { this: Module =>
  val req: DecoupledIO[ReqT]
  val resp: DecoupledIO[RespT]
}
