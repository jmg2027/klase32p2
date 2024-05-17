package klase32

import chisel3._
import chiseltest._
import klas.KlasTest
import klase32.config._
import klase32.param.DefaultConfig

class ALUTest extends KlasTest {
  implicit val p = new DefaultConfig

  def asUnsigned(x: Long) = {
    val prefix: Array[Byte] = Array(0)
    val byteArr = Seq.tabulate(4) { i =>
      ((x >> (i * 8)) & 0xFF).toByte
    }.reverse
    BigInt(prefix ++ byteArr)
  }

  behavior of "ALU"

  it should "perform various operations correctly under diverse scenarios" in {
    test(new ALU) { c =>
      // Function to test ALU operations
      def testALUOperation(op: ALUControlIE.Type, a: Int, b: Int, expected: Option[Int], expectFlag: Option[Boolean] = None): Unit = {
        c.io.ctrl.poke(op)
        c.io.A.poke(Integer.toUnsignedLong(a).U(32.W))
        c.io.B.poke(Integer.toUnsignedLong(b).U(32.W))
        // deal with option type
        expectFlag.foreach(flag => c.io.F.expect(flag.B))
        expected.foreach(expected => c.io.R.expect(Integer.toUnsignedLong(expected).U(32.W)))
        c.clock.step()
      }

      // Testing each operation with multiple scenarios
      val testValues = Seq(
        (0, 0), (1, 1), (0, 1), (1, 0),
        (10, 1), (1, 10), (10, 10),
        (100, 1), (1, 100), (1000000, 1), (1, 1000000),
        (100, 100), (1000000, 1000000),
        (Int.MaxValue, 1), (1, Int.MaxValue), (Int.MinValue, 1), (1, Int.MinValue),
        (Int.MaxValue, Int.MinValue), (Int.MinValue, Int.MaxValue)
      )

      // Operation definitions
      val operations = Seq(
        ALUControlIE.ADD,
        ALUControlIE.SUB, ALUControlIE.SLL, ALUControlIE.SRL,
        ALUControlIE.SRA, ALUControlIE.AND, ALUControlIE.OR, ALUControlIE.XOR,
        ALUControlIE.EQ,
        ALUControlIE.NE, ALUControlIE.LTU, ALUControlIE.LT,
        ALUControlIE.GEU, ALUControlIE.GE
      )

      for ((op, vals) <- operations.flatMap(op => testValues.map(vals => (op, vals)))) {
        val (a, b) = vals
        val (expected, expectFlag): (Option[Int], Option[Boolean]) = op match {
          case ALUControlIE.ADD => (Some(a + b), None)
          case ALUControlIE.SUB => (Some(a - b), None)
          case ALUControlIE.SLL => (Some(a << (b & 31)), None)
          case ALUControlIE.SRL => (Some(a >>> (b & 31)), None)
          case ALUControlIE.SRA => (Some(a >> (b & 31)), None)
          case ALUControlIE.AND => (Some(a & b), None)
          case ALUControlIE.OR  => (Some(a | b), None)
          case ALUControlIE.XOR => (Some(a ^ b), None)
          case ALUControlIE.EQ  => (None, Some(if (a == b) true else false))
          case ALUControlIE.NE  => (None, Some(if (a != b) true else false))
          case ALUControlIE.LTU => (None, Some(if (Integer.toUnsignedLong(a) < Integer.toUnsignedLong(b)) true else false))
          case ALUControlIE.LT  => (None, Some(if (a < b) true else false))
          case ALUControlIE.GEU => (None, Some(if (Integer.toUnsignedLong(a) >= Integer.toUnsignedLong(b)) true else false))
          case ALUControlIE.GE  => (None, Some(if (a >= b) true else false))
          case _ => (Some(0), None)
        }
        testALUOperation(op, a, b, expected, expectFlag)
      }
    }
  }
}
