package klase32.unit

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import klase32.config._
import klase32.param.KlasE32ParamKey

class DecoderTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Decoder"

  it should "correctly decode various instruction types" in {
    implicit val p: Parameters = new DefaultConfig() // Ensure this matches your configuration class

    test(new Decoder) { d =>
      // Function to test an instruction
      def testInstruction(inst: UInt, expected: Decoded): Unit = {
        d.io.inst.poke(inst)
        d.clock.step(1) // Simulate a clock cycle

        // Check each field in the Decoded bundle
        d.io.decSig.imm.i.expect(expected.imm.i)
        d.io.decSig.imm.u.expect(expected.imm.u)
        d.io.decSig.imm.j.expect(expected.imm.j)
        d.io.decSig.imm.b.expect(expected.imm.b)
        d.io.decSig.imm.s.expect(expected.imm.s)

        d.io.decSig.rd.expect(expected.rd)
        d.io.decSig.rs1.expect(expected.rs1)
        d.io.decSig.rs2.expect(expected.rs2)

        d.io.decSig.aluCtrl.expect(expected.aluCtrl)
        d.io.decSig.csrCtrl.expect(expected.csrCtrl)

        d.io.decSig.ecall.expect(expected.ecall)
        d.io.decSig.ebreak.expect(expected.ebreak)
        d.io.decSig.mret.expect(expected.mret)

        d.io.decSig.operandSelect.a.expect(expected.operandSelect.a)
        d.io.decSig.operandSelect.b.expect(expected.operandSelect.b)

        d.io.decSig.lsuCtrl.lsSize.expect(expected.lsuCtrl.lsSize)
        d.io.decSig.lsuCtrl.isLoad.expect(expected.lsuCtrl.isLoad)
        d.io.decSig.lsuCtrl.isStore.expect(expected.lsuCtrl.isStore)
        d.io.decSig.lsuCtrl.isSigned.expect(expected.lsuCtrl.isSigned)

        // Additional checks for any other necessary fields
      }

      // Example test case for an ADD instruction (R-type)
      val addInst = "b0000000_00001_00010_000_00011_0110011".U // ADD x3, x1, x2
      val expectedAddDecoding = new Decoded(p) {
        imm.i := 0.S; imm.u := 0.S; imm.j := 0.S; imm.b := 0.S; imm.s := 0.S
        rd := 3.U; rs1 := 1.U; rs2 := 2.U
        aluCtrl := ALUControlIE.ADD
        csrCtrl := CSRInstMuxIE.NONE
        // Set other fields as expected
      }.Lit()

      testInstruction(addInst, expectedAddDecoding)

      // Additional test cases for I-type, S-type, B-type, U-type, J-type instructions
      // You can add a function to simplify instruction encoding if necessary
    }
  }
}
