package klase32

import chisel3._
import chiseltest._
import klas.KlasTest
import klase32.config._
import klase32.param.DefaultConfig
import chisel3.util.BitPat
import chisel3.util.BitPat.bitPatToUInt
import snitch.enums._

class DecoderTest extends KlasTest {
  behavior of "Decoder"

  it should "correctly decode various instruction types" in {
    implicit val p: Parameters = new DefaultConfig() // Ensure this matches your configuration class
    // Function to initialize a Decoded bundle with default values
    test(new Decoder) { dut =>
      def initializeDecoded(decoded: Decoded): Unit = {
        // Initialize UInt and SInt fields with 0
        decoded.rd := 0.U
        decoded.rs1 := 0.U
        decoded.rs2 := 0.U
        decoded.imm.i := 0.S
        decoded.imm.u := 0.S
        decoded.imm.j := 0.S
        decoded.imm.b := 0.S
        decoded.imm.s := 0.S

        // Initialize EnumType fields with their default values
        decoded.aluCtrl := ALUControlIE.default
        decoded.csrCtrl := CSRControl.default
        decoded.operandSelect.a := OperandType.default
        decoded.operandSelect.b := OperandType.default
        decoded.rdType := RdType.default
        decoded.w1Wb := W1WritebackIE.default
        decoded.frontendCtrl := FrontendControlIE.default
        decoded.lsuCtrl.lsSize := DataSize.default
        decoded.lsuCtrl.isStore := false.B
        decoded.lsuCtrl.isLoad := false.B
        decoded.lsuCtrl.isSigned := SignedControl.default
        decoded.ecall := EcallIE.default
        decoded.ebreak := EbreakIE.default
        decoded.mret := MRetIE.default
        decoded.fence := FenceEnableIE.default
        decoded.flushICache := IcacheFlushIE.default
        decoded.wfi := WFIIE.default
        decoded.illegal := IllegalInstIE.default
      }

      // Initialize the Decoded bundle
      val defaultValues = Wire(new Decoded)
      initializeDecoded(defaultValues)


      //   // Function to generate a default field map
      //   def defaultFieldMap(decoded: Decoded): Map[String, UInt] = {
      //     def traverseBundle(bundle: Bundle, prefix: String = ""): Map[String, UInt] = {
      //       bundle.elements.flatMap {
      //         case (k, v) =>
      //           val fullName = if (prefix.isEmpty) k else s"$prefix.$k"
      //           v match {
      //             case u: UInt => Some(fullName -> 0.U(u.getWidth.W))
      //             case s: SInt => Some(fullName -> 0.S(s.getWidth.W).asUInt)
      //             case e: EnumType => Some(fullName -> e.cloneType.all.head.asUInt) // Use the first value as default
      //             case b: Bundle => traverseBundle(b, fullName)
      //             case _ => None
      //           }
      //       }.toMap
      //     }
      //     traverseBundle(decoded)
      //   }

      //   // Create a default field map for the Decoded bundle
      //   val defaultValues = defaultFieldMap(d.io.decSig)

      // Function to test an instruction
      def testInstruction(inst: BitPat, expectedFields: Map[String, UInt]): Unit = {
        dut.io.inst.poke(bitPatToUInt(inst))
        dut.clock.step() // Simulate a clock cycle

        val allFields = defaultValues.elements ++ expectedFields // Merge default fields with the fields to check

        allFields.foreach { case (field, expected) =>
          field.split('.').toList match {
            case List(k) =>
              dut.io.decSig.elements.get(k) match {
                case Some(_: UInt) => dut.io.decSig.elements(k).asInstanceOf[UInt].expect(expected.asInstanceOf[UInt])
                case Some(_: SInt) => dut.io.decSig.elements(k).asInstanceOf[SInt].asUInt.expect(expected.asInstanceOf[UInt])
                case Some(_: EnumType) => dut.io.decSig.elements(k).asInstanceOf[EnumType].asUInt.expect(expected.asInstanceOf[UInt])
                case _ => throw new Exception(s"Unknown field: $k")
              }
            case List(parent, child) =>
              dut.io.decSig.elements.get(parent) match {
                case Some(b: Bundle) =>
                  b.elements.get(child) match {
                    case Some(_: UInt) => b.elements(child).asInstanceOf[UInt].expect(expected.asInstanceOf[UInt])
                    case Some(_: SInt) => b.elements(child).asInstanceOf[SInt].asUInt.expect(expected.asInstanceOf[UInt])
                    case Some(_: EnumType) => b.elements(child).asInstanceOf[EnumType].asUInt.expect(expected.asInstanceOf[UInt])
                    case _ => throw new Exception(s"Unknown field: $parent.$child")
                  }
                case _ => throw new Exception(s"Unknown field: $parent")
              }
            case _ => throw new Exception(s"Invalid field: $field")
          }
        }
      }

      val instructionTests = Seq(
        (Instructions.ADD, Map("aluCtrl" -> ALUControlIE.ADD.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.Reg.asUInt)),
        (Instructions.ADDI, Map("aluCtrl" -> ALUControlIE.ADD.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.IImmediate.asUInt)),
        (Instructions.SUB, Map("aluCtrl" -> ALUControlIE.SUB.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.Reg.asUInt)),
        (Instructions.XOR, Map("aluCtrl" -> ALUControlIE.XOR.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.Reg.asUInt)),
        (Instructions.XORI, Map("aluCtrl" -> ALUControlIE.XOR.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.IImmediate.asUInt)),
        (Instructions.OR, Map("aluCtrl" -> ALUControlIE.OR.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.Reg.asUInt)),
        (Instructions.ORI, Map("aluCtrl" -> ALUControlIE.OR.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.IImmediate.asUInt)),
        (Instructions.AND, Map("aluCtrl" -> ALUControlIE.AND.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.Reg.asUInt)),
        (Instructions.ANDI, Map("aluCtrl" -> ALUControlIE.AND.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.IImmediate.asUInt)),
        (Instructions.SLT, Map("aluCtrl" -> ALUControlIE.SLT.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.Reg.asUInt)),
        (Instructions.SLTI, Map("aluCtrl" -> ALUControlIE.SLT.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.IImmediate.asUInt)),
        (Instructions.SLTU, Map("aluCtrl" -> ALUControlIE.SLTU.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.Reg.asUInt)),
        (Instructions.SLTIU, Map("aluCtrl" -> ALUControlIE.SLTU.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.IImmediate.asUInt)),
        (Instructions.SLL, Map("aluCtrl" -> ALUControlIE.SLL.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.Reg.asUInt)),
        (Instructions.SLLI, Map("aluCtrl" -> ALUControlIE.SLL.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.IImmediate.asUInt)),
        (Instructions.SRL, Map("aluCtrl" -> ALUControlIE.SRL.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.Reg.asUInt)),
        (Instructions.SRLI, Map("aluCtrl" -> ALUControlIE.SRL.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.IImmediate.asUInt)),
        (Instructions.SRA, Map("aluCtrl" -> ALUControlIE.SRA.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.Reg.asUInt)),
        (Instructions.SRAI, Map("aluCtrl" -> ALUControlIE.SRA.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.IImmediate.asUInt)),

        (Instructions.LUI, Map("aluCtrl" -> ALUControlIE.ADD.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.UImmediate.asUInt)),
        (Instructions.AUIPC, Map("aluCtrl" -> ALUControlIE.ADD.asUInt, "operandSelect.a" -> OperandType.PC.asUInt, "operandSelect.b" -> OperandType.UImmediate.asUInt)),

        (Instructions.JAL, Map("operandSelect.a" -> OperandType.PC.asUInt, "operandSelect.b" -> OperandType.JImmediate.asUInt, "rdType" -> RdType.ConsecPC.asUInt, "frontendCtrl" -> FrontendControlIE.JAL.asUInt, "w1Wb" -> W1WritebackIE.EN.asUInt, "nextPc" -> PcType.Alu.asUInt)),
        (Instructions.JALR, Map("operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.IImmediate.asUInt, "rdType" -> RdType.ConsecPC.asUInt, "frontendCtrl" -> FrontendControlIE.JALR.asUInt, "nextPc" -> PcType.Alu.asUInt)),

        (Instructions.BEQ, Map("aluCtrl" -> ALUControlIE.EQ.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.Reg.asUInt, "nextPc" -> PcType.Branch.asUInt, "writeRd" -> false.B)),
        (Instructions.BNE, Map("aluCtrl" -> ALUControlIE.NE.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.Reg.asUInt, "nextPc" -> PcType.Branch.asUInt, "writeRd" -> false.B)),
        (Instructions.BLT, Map("aluCtrl" -> ALUControlIE.SLT.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.Reg.asUInt, "nextPc" -> PcType.Branch.asUInt, "writeRd" -> false.B)),
        (Instructions.BLTU, Map("aluCtrl" -> ALUControlIE.SLTU.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.Reg.asUInt, "nextPc" -> PcType.Branch.asUInt, "writeRd" -> false.B)),
        (Instructions.BGE, Map("aluCtrl" -> ALUControlIE.GE.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.Reg.asUInt, "nextPc" -> PcType.Branch.asUInt, "writeRd" -> false.B)),
        (Instructions.BGEU, Map("aluCtrl" -> ALUControlIE.GEU.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.Reg.asUInt, "nextPc" -> PcType.Branch.asUInt, "writeRd" -> false.B)),

        (Instructions.SB, Map("lsuCtrl.lsSize" -> DataSize.Byte.asUInt, "lsuCtrl.isStore" -> StoreControl.EN.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.SImmediate.asUInt, "writeRd" -> false.B)),
        (Instructions.SH, Map("lsuCtrl.lsSize" -> DataSize.HalfWord.asUInt, "lsuCtrl.isStore" -> StoreControl.EN.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.SImmediate.asUInt, "writeRd" -> false.B)),
        (Instructions.SW, Map("lsuCtrl.lsSize" -> DataSize.Word.asUInt, "lsuCtrl.isStore" -> StoreControl.EN.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.SImmediate.asUInt, "writeRd" -> false.B)),

        (Instructions.LB, Map("lsuCtrl.lsSize" -> DataSize.Byte.asUInt, "lsuCtrl.isLoad" -> LoadControl.EN.asUInt, "lsuCtrl.isSigned" -> SignedControl.signed.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.IImmediate.asUInt, "useRd" -> true.B)),
        (Instructions.LH, Map("lsuCtrl.lsSize" -> DataSize.HalfWord.asUInt, "lsuCtrl.isLoad" -> LoadControl.EN.asUInt, "lsuCtrl.isSigned" -> SignedControl.signed.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.IImmediate.asUInt, "useRd" -> true.B)),
        (Instructions.LW, Map("lsuCtrl.lsSize" -> DataSize.Word.asUInt, "lsuCtrl.isLoad" -> LoadControl.EN.asUInt, "lsuCtrl.isSigned" -> SignedControl.signed.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.IImmediate.asUInt, "useRd" -> true.B)),
        (Instructions.LBU, Map("lsuCtrl.lsSize" -> DataSize.Byte.asUInt, "lsuCtrl.isLoad" -> LoadControl.EN.asUInt, "lsuCtrl.isSigned" -> SignedControl.unsigned.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.IImmediate.asUInt, "useRd" -> true.B)),
        (Instructions.LHU, Map("lsuCtrl.lsSize" -> DataSize.HalfWord.asUInt, "lsuCtrl.isLoad" -> LoadControl.EN.asUInt, "lsuCtrl.isSigned" -> SignedControl.unsigned.asUInt, "operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.IImmediate.asUInt, "useRd" -> true.B)),

        (Instructions.CSRRW, Map("operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.None.asUInt, "csrCtrl" -> CSRControl.RW.asUInt, "rdType" -> RdType.BypassCSR.asUInt)),
        (Instructions.CSRRWI, Map("operandSelect.a" -> OperandType.CSRImmediate.asUInt, "operandSelect.b" -> OperandType.None.asUInt, "csrCtrl" -> CSRControl.RW.asUInt, "rdType" -> RdType.BypassCSR.asUInt)),
        (Instructions.CSRRS, Map("operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.None.asUInt, "csrCtrl" -> CSRControl.RS.asUInt, "rdType" -> RdType.BypassCSR.asUInt)),
        (Instructions.CSRRSI, Map("operandSelect.a" -> OperandType.CSRImmediate.asUInt, "operandSelect.b" -> OperandType.None.asUInt, "csrCtrl" -> CSRControl.RS.asUInt, "rdType" -> RdType.BypassCSR.asUInt)),
        (Instructions.CSRRC, Map("operandSelect.a" -> OperandType.Reg.asUInt, "operandSelect.b" -> OperandType.None.asUInt, "csrCtrl" -> CSRControl.RC.asUInt, "rdType" -> RdType.BypassCSR.asUInt)),
        (Instructions.CSRRCI, Map("operandSelect.a" -> OperandType.CSRImmediate.asUInt, "operandSelect.b" -> OperandType.None.asUInt, "csrCtrl" -> CSRControl.RC.asUInt, "rdType" -> RdType.BypassCSR.asUInt)),

        (Instructions.ECALL, Map("ecall" -> EcallIE.EN.asUInt)),
        (Instructions.EBREAK, Map("ebreak" -> EbreakIE.EN.asUInt)),

        (Instructions.MRET, Map("frontendCtrl" -> FrontendControlIE.MRET.asUInt, "mret" -> MRetIE.EN.asUInt, "nextPc" -> PcType.MRet.asUInt)),

        (Instructions.FENCE, Map("fence" -> FenceEnableIE.EN.asUInt)),
        (Instructions.FENCE_I, Map("flushICache" -> IcacheFlushIE.EN.asUInt)),
        (Instructions.WFI, Map("wfi" -> WFIIE.EN.asUInt))
      )

      // Run all the tests
      for ((inst, expectedFields) <- instructionTests) {
        testInstruction(inst, expectedFields)
      }

      // Example test case for an ADD instruction (R-type)
      val addInst = BitPat("b0000000_00001_00010_000_00011_0110011") // ADD x3, x1, x2
      val addExpectedFields = Map(
        "rd" -> 3.U,
        "rs1" -> 1.U,
        "rs2" -> 2.U,
        "aluCtrl" -> ALUControlIE.ADD.asUInt,
        "operandSelect.a" -> OperandType.Reg.asUInt,
        "operandSelect.b" -> OperandType.Reg.asUInt
      )
      testInstruction(addInst, addExpectedFields)

      // Example test case for an ADDI instruction (I-type)
      val addiInst = BitPat("b000000000001_00001_000_00010_0010011".U) // ADDI x2, x1, 1
      val addiExpectedFields = Map(
        "rd" -> 2.U,
        "rs1" -> 1.U,
        "imm.i" -> 1.S.asUInt,
        "aluCtrl" -> ALUControlIE.ADD.asUInt,
        "operandSelect.a" -> OperandType.Reg.asUInt,
        "operandSelect.b" -> OperandType.IImmediate.asUInt
      )
      testInstruction(addiInst, addiExpectedFields)

      // Example test case for an SW instruction (S-type)
      val swInst = BitPat("b0000000_00010_00001_010_00011_0100011") // SW x2, 3(x1)
      val swExpectedFields = Map(
        "rs1" -> 1.U,
        "rs2" -> 2.U,
        "imm.s" -> 3.S.asUInt,
        "lsuCtrl.lsSize" -> DataSize.Word.asUInt,
        "lsuCtrl.isStore" -> true.B.asUInt,
        "operandSelect.a" -> OperandType.Reg.asUInt,
        "operandSelect.b" -> OperandType.SImmediate.asUInt
      )
      testInstruction(swInst, swExpectedFields)

      // Example test case for an LUI instruction (U-type)
      val luiInst = BitPat("b00000000000100000000_00000_0110111") // LUI x0, 0x1000
      val luiExpectedFields = Map(
        "rd" -> 0.U,
        "imm.u" -> 0x1000.S.asUInt,
        "aluCtrl" -> ALUControlIE.ADD.asUInt,
        "operandSelect.a" -> OperandType.Reg.asUInt,
        "operandSelect.b" -> OperandType.UImmediate.asUInt
      )
      testInstruction(luiInst, luiExpectedFields)

      // Example test case for a JAL instruction (J-type)
      val jalInst = BitPat("b00000000000100000000_00000_1101111") // JAL x0, 0x1000
      val jalExpectedFields = Map(
        "rd" -> 0.U,
        "imm.j" -> 0x1000.S.asUInt,
        "frontendCtrl" -> FrontendControlIE.JAL.asUInt,
        "w1Wb" -> W1WritebackIE.EN.asUInt,
        "operandSelect.a" -> OperandType.PC.asUInt,
        "operandSelect.b" -> OperandType.JImmediate.asUInt
      )
      testInstruction(jalInst, jalExpectedFields)

      // Example test case for a illegal instruction
      val illegalInst = BitPat("b1")
      val illegalExpectedFields = Map(
        "illegal" -> IllegalInstIE.EN.asUInt
      )
      testInstruction(illegalInst, illegalExpectedFields)
    }
  }
}
