package snitch.test

import scala.collection.{mutable => mut}

import assembler.RISCVAssembler

class TestMem {
  val mem = mut.Map[BigInt, BigInt]()

  def alignAddr(addr: BigInt) = ((addr >> 2) << 2, addr & 3)

  def load(addr: BigInt) = {
    val (aligned, offset) = alignAddr(addr)
    val ret = mem.get(aligned).getOrElse(BigInt(0))
    ret >> (offset.toInt * 8)
  }

  def store(addr: BigInt, v: BigInt) = {
    val (aligned, offset) = alignAddr(addr)
    mem(aligned) = v << (offset.toInt * 8)
  }

  def hexToBigInt(hex: String): BigInt = {
    hex.toLowerCase().toList.map {
      "0123456789abcdef".indexOf(_)
    }.map(BigInt(_)).reduceLeft(_ * 16 + _)
  }

  def setup(start: BigInt, bin: Seq[BigInt]) = {
    Seq.tabulate(bin.size)(start + _*4)
      .zip(bin)
      .foreach { case (a, v) =>
        println(f"[Mem] Setup Request addr($a%X) data($v%X)")
        mem(a) = v
        store(a, v)
      }
  }

  def setupText(start: BigInt, text: String) = {
    val bin = RISCVAssembler.fromString(text).trim.split("\n").map(hexToBigInt(_))
    setup(start, bin.toSeq)
  }

  def setupData(start: BigInt, data: String) = {
    val bin = data.trim.split("\n").map(hexToBigInt(_))
    setup(start, bin.toSeq)
  }

  def dump(addrs: Seq[BigInt]) = addrs.map { x => load(x) }
}
