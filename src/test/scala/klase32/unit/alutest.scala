package klase32

import chisel3._
import chiseltest._
import klas.KlasTest
import klase32.config._
import klase32.param.DefaultConfig

class ALUTest extends KlasTest {
  implicit val p = new DefaultConfig

  behavior of "ALU"

  it should "perform various operations correctly under diverse scenarios" in {
    test(new ALU) { dut =>
      // Function to test ALU operations
      def testALUOperation(op: ALUControlIE.Type, a: Int, b: Int, expected: Option[Int], expectFlag: Option[Boolean] = None): Unit = {
        dut.io.ctrl.poke(op)
        dut.io.A.poke(Integer.toUnsignedLong(a).U(32.W))
        dut.io.B.poke(Integer.toUnsignedLong(b).U(32.W))
        // deal with option type
        expectFlag.foreach(flag => dut.io.F.expect(flag.B))
        expected.foreach(expected => dut.io.R.expect(Integer.toUnsignedLong(expected).U(32.W)))
        dut.clock.step()
      }

      def getExpectedAndFlag(op: ALUControlIE.Type, a: Int, b: Int): (Option[Int], Option[Boolean]) = op match {
        case ALUControlIE.ADD => (Some(a + b), None)
        case ALUControlIE.SUB => (Some(a - b), None)
        case ALUControlIE.SLL => (Some(a << (b & 31)), None)
        case ALUControlIE.SRL => (Some(a >>> (b & 31)), None)
        case ALUControlIE.SRA => (Some(a >> (b & 31)), None)
        case ALUControlIE.AND => (Some(a & b), None)
        case ALUControlIE.OR  => (Some(a | b), None)
        case ALUControlIE.XOR => (Some(a ^ b), None)
        case ALUControlIE.EQ  => (None, Some(a == b))
        case ALUControlIE.NE  => (None, Some(a != b))
        case ALUControlIE.LTU => (None, Some(Integer.toUnsignedLong(a) < Integer.toUnsignedLong(b)))
        case ALUControlIE.LT  => (None, Some(a < b))
        case ALUControlIE.GEU => (None, Some(Integer.toUnsignedLong(a) >= Integer.toUnsignedLong(b)))
        case ALUControlIE.GE  => (None, Some(a >= b))
        case _ => (Some(0), None)
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
        val (expected, expectFlag) = getExpectedAndFlag(op, a, b)
        testALUOperation(op, a, b, expected, expectFlag)
      }

      // Adding random tests
      for (_ <- 0 until 100) {
        val a = Random.nextInt()
        val b = Random.nextInt()
        for (op <- operations) {
          val (expected, expectFlag) = getExpectedAndFlag(op, a, b)
          testALUOperation(op, a, b, expected, expectFlag)
        }
      }
    }
  }
}
