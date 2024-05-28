package klase32

import chisel3._
import chiseltest._
import klas.KlasTest
import klase32.config._
import klase32.param.DefaultConfig
import chisel3.util.BitPat
import chisel3.util.BitPat.bitPatToUInt
import snitch.enums._
import freechips.rocketchip.diplomacy.BufferParams.default

class DecoderTest extends KlasTest {
  behavior of "Decoder"

  it should "correctly decode various instruction types" in {
    implicit val p: Parameters = new DefaultConfig() // Ensure this matches your configuration class
    // Function to initialize a Decoded bundle with default values
    test(new Decoder) { dut =>
      val defaultValues = Map{
        // Initialize UInt and SInt fields with 0
        "rd" -> 0.U
        "rs1" -> 0.U
        "rs2" -> 0.U
        "imm.i" -> 0.S
        "imm.u" -> 0.S
        "imm.j" -> 0.S
        "imm.b" -> 0.S
        "imm.s" -> 0.S

        // Initialize EnumType fields with their default values
        "aluCtrl" -> ALUControlIE.default
        "csrCtrl" -> CSRControl.default
        "operandSelect.a" -> OperandType.default
        "operandSelect.b" -> OperandType.default
        "rdType" -> RdType.default
        "w1Wb" -> W1WritebackIE.default
        "frontendCtrl" -> FrontendControlIE.default
        "lsuCtrl.lsSize" -> DataSize.default
        "lsuCtrl.isStore" -> false.B
        "lsuCtrl.isLoad" -> false.B
        "lsuCtrl.isSigned" -> SignedControl.default
        "ecall" -> EcallIE.default
        "ebreak" -> EbreakIE.default
        "mret" -> MRetIE.default
        "fence" -> FenceEnableIE.default
        "flushICache" -> IcacheFlushIE.default
        "wfi" -> WFIIE.default
        "illegal" -> IllegalInstIE.default
      }

//
//      // Function to generate a default field map
//      def defaultFieldMap(decoded: Decoded): Map[String, Data] = {
//        def traverseBundle(bundle: Bundle, prefix: String = ""): Map[String, Data] = {
//          bundle.elements.flatMap {
//            case (k, v) =>
//              val fullName = if (prefix.isEmpty) k else s"$prefix.$k"
//              v match {
//                case u: UInt => Some(fullName -> 0.U(u.getWidth.W))
//                case s: SInt => Some(fullName -> 0.S(s.getWidth.W))
//                case e: ControlDefaultEnum#Type => Some(fullName -> 0.U.asTypeOf(e))
//                case b: Bundle => traverseBundle(b, fullName)
//                case _ => None
//              }
//          }.toMap
//        }
//        traverseBundle(decoded)
//      }
//
//      // Create a default field map for the Decoded bundle
//      val defaultValues = defaultFieldMap(dut.io.decSig)
//      println(defaultValues)

      // Function to test an instruction
      def testInstruction(inst: BitPat, expectedFields: Map[String, Data]): Unit = {
        println(expectedFields)
        dut.io.inst.poke(inst.value)
        dut.clock.step() // Simulate a clock cycle

        val allFields = defaultValues ++ expectedFields // Merge default fields with the fields to check

        allFields.foreach { case (field, expected) =>
          field.split('.').toList match {
            case List(k) =>
              dut.io.decSig.elements.get(k) match {
                case Some(_: UInt) => dut.io.decSig.elements(k).asInstanceOf[UInt].expect(expected.asInstanceOf[UInt])
                case Some(_: SInt) => dut.io.decSig.elements(k).asInstanceOf[SInt].expect(expected.asInstanceOf[SInt])
                case Some(_: EnumType) => dut.io.decSig.elements(k).asInstanceOf[EnumType].expect(expected.asInstanceOf[EnumType])
                case _ => throw new Exception(s"Unknown field: $k")
              }
            case List(parent, child) =>
              dut.io.decSig.elements.get(parent) match {
                case Some(b: Bundle) =>
                  b.elements.get(child) match {
                    case Some(_: UInt) => b.elements(child).asInstanceOf[UInt].expect(expected.asInstanceOf[UInt])
                    case Some(_: SInt) => b.elements(child).asInstanceOf[SInt].expect(expected.asInstanceOf[SInt])
                    case Some(_: EnumType) => b.elements(child).asInstanceOf[EnumType].expect(expected.asInstanceOf[EnumType])
                    case _ => throw new Exception(s"Unknown field: $parent.$child")
                  }
                case _ => throw new Exception(s"Unknown field: $parent")
              }
            case _ => throw new Exception(s"Invalid field: $field")
          }
        }
      }

      val instructionTests = Seq(
        (Instructions.ADD, Map("aluCtrl" -> ALUControlIE.ADD, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.Reg)),
        (Instructions.ADDI, Map("aluCtrl" -> ALUControlIE.ADD, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.IImmediate)),
        (Instructions.SUB, Map("aluCtrl" -> ALUControlIE.SUB, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.Reg)),
        (Instructions.XOR, Map("aluCtrl" -> ALUControlIE.XOR, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.Reg)),
        (Instructions.XORI, Map("aluCtrl" -> ALUControlIE.XOR, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.IImmediate)),
        (Instructions.OR, Map("aluCtrl" -> ALUControlIE.OR, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.Reg)),
        (Instructions.ORI, Map("aluCtrl" -> ALUControlIE.OR, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.IImmediate)),
        (Instructions.AND, Map("aluCtrl" -> ALUControlIE.AND, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.Reg)),
        (Instructions.ANDI, Map("aluCtrl" -> ALUControlIE.AND, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.IImmediate)),
        (Instructions.SLT, Map("aluCtrl" -> ALUControlIE.SLT, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.Reg)),
        (Instructions.SLTI, Map("aluCtrl" -> ALUControlIE.SLT, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.IImmediate)),
        (Instructions.SLTU, Map("aluCtrl" -> ALUControlIE.SLTU, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.Reg)),
        (Instructions.SLTIU, Map("aluCtrl" -> ALUControlIE.SLTU, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.IImmediate)),
        (Instructions.SLL, Map("aluCtrl" -> ALUControlIE.SLL, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.Reg)),
        (Instructions.SLLI, Map("aluCtrl" -> ALUControlIE.SLL, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.IImmediate)),
        (Instructions.SRL, Map("aluCtrl" -> ALUControlIE.SRL, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.Reg)),
        (Instructions.SRLI, Map("aluCtrl" -> ALUControlIE.SRL, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.IImmediate)),
        (Instructions.SRA, Map("aluCtrl" -> ALUControlIE.SRA, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.Reg)),
        (Instructions.SRAI, Map("aluCtrl" -> ALUControlIE.SRA, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.IImmediate)),

        (Instructions.LUI, Map("aluCtrl" -> ALUControlIE.ADD, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.UImmediate)),
        (Instructions.AUIPC, Map("aluCtrl" -> ALUControlIE.ADD, "operandSelect.a" -> OperandType.PC, "operandSelect.b" -> OperandType.UImmediate)),

        (Instructions.JAL, Map("operandSelect.a" -> OperandType.PC, "operandSelect.b" -> OperandType.JImmediate, "rdType" -> RdType.ConsecPC, "frontendCtrl" -> FrontendControlIE.JAL, "w1Wb" -> W1WritebackIE.EN)),
        (Instructions.JALR, Map("operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.IImmediate, "rdType" -> RdType.ConsecPC, "frontendCtrl" -> FrontendControlIE.JALR)),

        (Instructions.BEQ, Map("aluCtrl" -> ALUControlIE.EQ, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.Reg, "frontendCtrl" -> FrontendControlIE.BR, "w1Wb" -> W1WritebackIE.EN)),
        (Instructions.BNE, Map("aluCtrl" -> ALUControlIE.NE, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.Reg, "frontendCtrl" -> FrontendControlIE.BR, "w1Wb" -> W1WritebackIE.EN)),
        (Instructions.BLT, Map("aluCtrl" -> ALUControlIE.SLT, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.Reg, "frontendCtrl" -> FrontendControlIE.BR, "w1Wb" -> W1WritebackIE.EN)),
        (Instructions.BLTU, Map("aluCtrl" -> ALUControlIE.SLTU, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.Reg, "frontendCtrl" -> FrontendControlIE.BR, "w1Wb" -> W1WritebackIE.EN)),
        (Instructions.BGE, Map("aluCtrl" -> ALUControlIE.GE, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.Reg, "frontendCtrl" -> FrontendControlIE.BR, "w1Wb" -> W1WritebackIE.EN)),
        (Instructions.BGEU, Map("aluCtrl" -> ALUControlIE.GEU, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.Reg, "frontendCtrl" -> FrontendControlIE.BR, "w1Wb" -> W1WritebackIE.EN)),

        (Instructions.SB, Map("lsuCtrl.lsSize" -> DataSize.Byte, "lsuCtrl.isStore" -> StoreControl.EN, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.SImmediate, "w1Wb" -> W1WritebackIE.EN)),
        (Instructions.SH, Map("lsuCtrl.lsSize" -> DataSize.HalfWord, "lsuCtrl.isStore" -> StoreControl.EN, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.SImmediate, "w1Wb" -> W1WritebackIE.EN)),
        (Instructions.SW, Map("lsuCtrl.lsSize" -> DataSize.Word, "lsuCtrl.isStore" -> StoreControl.EN, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.SImmediate, "w1Wb" -> W1WritebackIE.EN)),

        (Instructions.LB, Map("lsuCtrl.lsSize" -> DataSize.Byte, "lsuCtrl.isLoad" -> LoadControl.EN, "lsuCtrl.isSigned" -> SignedControl.signed, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.IImmediate)),
        (Instructions.LH, Map("lsuCtrl.lsSize" -> DataSize.HalfWord, "lsuCtrl.isLoad" -> LoadControl.EN, "lsuCtrl.isSigned" -> SignedControl.signed, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.IImmediate)),
        (Instructions.LW, Map("lsuCtrl.lsSize" -> DataSize.Word, "lsuCtrl.isLoad" -> LoadControl.EN, "lsuCtrl.isSigned" -> SignedControl.signed, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.IImmediate)),
        (Instructions.LBU, Map("lsuCtrl.lsSize" -> DataSize.Byte, "lsuCtrl.isLoad" -> LoadControl.EN, "lsuCtrl.isSigned" -> SignedControl.unsigned, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.IImmediate)),
        (Instructions.LHU, Map("lsuCtrl.lsSize" -> DataSize.HalfWord, "lsuCtrl.isLoad" -> LoadControl.EN, "lsuCtrl.isSigned" -> SignedControl.unsigned, "operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.IImmediate)),

        (Instructions.CSRRW, Map("operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.None, "csrCtrl" -> CSRControl.RW, "rdType" -> RdType.BypassCSR)),
        (Instructions.CSRRWI, Map("operandSelect.a" -> OperandType.CSRImmediate, "operandSelect.b" -> OperandType.None, "csrCtrl" -> CSRControl.RW, "rdType" -> RdType.BypassCSR)),
        (Instructions.CSRRS, Map("operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.None, "csrCtrl" -> CSRControl.RS, "rdType" -> RdType.BypassCSR)),
        (Instructions.CSRRSI, Map("operandSelect.a" -> OperandType.CSRImmediate, "operandSelect.b" -> OperandType.None, "csrCtrl" -> CSRControl.RS, "rdType" -> RdType.BypassCSR)),
        (Instructions.CSRRC, Map("operandSelect.a" -> OperandType.Reg, "operandSelect.b" -> OperandType.None, "csrCtrl" -> CSRControl.RC, "rdType" -> RdType.BypassCSR)),
        (Instructions.CSRRCI, Map("operandSelect.a" -> OperandType.CSRImmediate, "operandSelect.b" -> OperandType.None, "csrCtrl" -> CSRControl.RC, "rdType" -> RdType.BypassCSR)),

        (Instructions.ECALL, Map("ecall" -> EcallIE.EN)),
        (Instructions.EBREAK, Map("ebreak" -> EbreakIE.EN)),

        (Instructions.MRET, Map("frontendCtrl" -> FrontendControlIE.MRET, "mret" -> MRetIE.EN, "w1Wb" -> W1WritebackIE.EN)),

        (Instructions.FENCE, Map("fence" -> FenceEnableIE.EN, "w1Wb" -> W1WritebackIE.EN)),
        (Instructions.FENCE_I, Map("flushICache" -> IcacheFlushIE.EN)),
        (Instructions.WFI, Map("wfi" -> WFIIE.EN))
      )

      // Run all the tests
      for ((inst, expectedFields) <- instructionTests) {
        testInstruction(inst, expectedFields)
      }

      // Example test case for an ADD instruction (R-type)
      val addInst = BitPat("b0000000_00010_00001_000_00011_0110011") // ADD x3, x1, x2
      val addExpectedFields = Map(
        "rd" -> 3.U,
        "rs1" -> 1.U,
        "rs2" -> 2.U,
        "aluCtrl" -> ALUControlIE.ADD,
        "operandSelect.a" -> OperandType.Reg,
        "operandSelect.b" -> OperandType.Reg
      )
      testInstruction(addInst, addExpectedFields)

      // Example test case for an ADDI instruction (I-type)
      val addiInst = BitPat("b000000000001_00001_000_00010_0010011") // ADDI x2, x1, 1
      val addiExpectedFields = Map(
        "rd" -> 2.U,
        "rs1" -> 1.U,
        "imm.i" -> 1.S,
        "aluCtrl" -> ALUControlIE.ADD,
        "operandSelect.a" -> OperandType.Reg,
        "operandSelect.b" -> OperandType.IImmediate
      )
      testInstruction(addiInst, addiExpectedFields)

      // Example test case for an SW instruction (S-type)
      val swInst = BitPat("b0000000_00010_00001_010_00011_0100011") // SW x2, 3(x1)
      val swExpectedFields = Map(
        "rs1" -> 1.U,
        "rs2" -> 2.U,
        "imm.s" -> 3.S,
        "lsuCtrl.lsSize" -> DataSize.Word,
        "lsuCtrl.isStore" -> StoreControl.EN,
        "operandSelect.a" -> OperandType.Reg,
        "operandSelect.b" -> OperandType.SImmediate
      )
      testInstruction(swInst, swExpectedFields)

      // Example test case for an LUI instruction (U-type)
      val luiInst = BitPat("b00000000000000000001_00000_0110111") // LUI x0, 0x1000
      val luiExpectedFields = Map(
        "rd" -> 0.U,
        "imm.u" -> 0x1000.S,
        "aluCtrl" -> ALUControlIE.ADD,
        "operandSelect.a" -> OperandType.Reg,
        "operandSelect.b" -> OperandType.UImmediate
      )
      testInstruction(luiInst, luiExpectedFields)

      // Example test case for a JAL instruction (J-type)
      val jalInst = BitPat("b0_0000000000_0_00000001_00000_1101111") // JAL x0, 0x1000
      val jalExpectedFields = Map(
        "rd" -> 0.U,
        "imm.j" -> 0x1000.S,
        "frontendCtrl" -> FrontendControlIE.JAL,
        "w1Wb" -> W1WritebackIE.EN,
        "operandSelect.a" -> OperandType.PC,
        "operandSelect.b" -> OperandType.JImmediate
      )
      testInstruction(jalInst, jalExpectedFields)

      // Example test case for a illegal instruction
      val illegalInst = BitPat("b1")
      val illegalExpectedFields = Map(
        "illegal" -> IllegalInstIE.illegal
      )
      testInstruction(illegalInst, illegalExpectedFields)
    }
  }
}
