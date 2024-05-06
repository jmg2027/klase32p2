package klase32.unit

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import klase32.config._
import klase32.param.KlasE32ParamKey

class ALUTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "ALU"

  it should "perform various operations correctly under diverse scenarios" in {
    implicit val p: Parameters = new DefaultConfig() // Ensure this matches your configuration class

    test(new ALU) { c =>
      // Function to test ALU operations
      def testALUOperation(op: ALUControlIE.Type, a: Int, b: Int, expected: Int, expectFlag: Option[Boolean] = None): Unit = {
        c.io.ctrl.poke(op)
        c.io.A.poke(a.U)
        c.io.B.poke(b.U)
        c.clock.step(1)
        c.io.R.expect(expected.U)
        expectFlag.foreach(flag => c.io.F.expect(flag.B))
      }

      // Testing each operation with multiple scenarios
      val testValues = Seq(
        (0, 0), (1, 1), (0, 1), (1, 0), (-1, 1), (1, -1), (-1, -1),
        (Int.MaxValue, 1), (1, Int.MaxValue), (Int.MinValue, 1), (1, Int.MinValue),
        (Int.MaxValue, Int.MinValue), (Int.MinValue, Int.MaxValue)
      )

      // Operation definitions
      val operations = Seq(
        ALUControlIE.ADD, ALUControlIE.SUB, ALUControlIE.SLL, ALUControlIE.SRL,
        ALUControlIE.SRA, ALUControlIE.AND, ALUControlIE.OR, ALUControlIE.XOR,
        ALUControlIE.EQ, ALUControlIE.NE, ALUControlIE.LTU, ALUControlIE.LT,
        ALUControlIE.GEU, ALUControlIE.GT
      )

      for ((op, vals) <- operations.flatMap(op => testValues.map(vals => (op, vals)))) {
        val (a, b) = vals
        val expected = op match {
          case ALUControlIE.ADD => a + b
          case ALUControlIE.SUB => a - b
          case ALUControlIE.SLL => a << (b & 31)
          case ALUControlIE.SRL => a >>> (b & 31)
          case ALUControlIE.SRA => a >> (b & 31)
          case ALUControlIE.AND => a & b
          case ALUControlIE.OR  => a | b
          case ALUControlIE.XOR => a ^ b
          case ALUControlIE.EQ  => if (a == b) 1 else 0
          case ALUControlIE.NE  => if (a != b) 1 else 0
          case ALUControlIE.LTU => if (Integer.toUnsignedLong(a) < Integer.toUnsignedLong(b)) 1 else 0
          case ALUControlIE.LT  => if (a < b) 1 else 0
          case ALUControlIE.GEU => if (Integer.toUnsignedLong(a) >= Integer.toUnsignedLong(b)) 1 else 0
          case ALUControlIE.GT  => if (a > b) 1 else 0
          case _ => 0
        }
        testALUOperation(op, a, b, expected)
      }
    }
  }
}