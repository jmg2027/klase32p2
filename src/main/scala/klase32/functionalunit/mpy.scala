package klase32.functionalunit

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import klase32.include.config._
import klase32.include.param.KLASE32ParamKey
import klase32.include.KLASE32AbstractClass._
import klase32.include.ControlSignal._
import klase32.common.FunctionUnitIO

class MPYReq(implicit p: Parameters) extends CoreBundle {
  val ctrl = MPYControlIE()
  val A = SInt(xLen.W)
  val B = SInt(xLen.W)
  val rd = UInt(regIdWidth.W)
}

class MPYResp(implicit p: Parameters) extends CoreBundle {
  val P = UInt(xLen.W)
  val rd = UInt(regIdWidth.W)
}

class MPY(implicit p: Parameters) extends CoreModule
with FunctionUnitIO[MPYReq, MPYResp] {
  val req = IO(Flipped(Decoupled(new MPYReq)))
  val resp = IO(Decoupled(new MPYResp))

  val core = Module(new IterativeAdderCore(MulParams(
    dataWidth = xLen,
    sliceWidth = 16,
    slicesPerCycle = 2)))
  core.io.packet.a  := req.bits.A.asUInt
  core.io.packet.b  := req.bits.B.asUInt
  core.io.packet.op := Cat(req.bits.ctrl.asUInt(1), req.bits.ctrl.asUInt(0)) // LUT
  core.io.start := req.fire
  core.io.kill  := flush        // precise flush

  req.ready   := !core.io.busy
  resp.valid  := core.io.done
  resp.bits.P := core.io.result
  resp.bits.rd:= req.bits.rd
}

/*******************************************************************************
 * Parameter-driven multi-cycle multiplier with carry-save reduction.
 *  • dataBits   : 32 / 64 / …  (multiple of 32)
 *  • sliceBits  :  16 / 32 (divides dataBits)
 *  • nSlice     : ≥ 1          (#partial products issued per cycle)
 *  • Supported  : MUL / MULH / MULHSU / MULHU
 *
 * Fast rules
 *  • MUL        : finishes when lower bit related PPs are complete
 *  • MULH*      : finishes when all PP are complete
 *  • fast-hit   : if upper half of both operands is 0
 *                 – MULH*  ⇒ result = 0   (0 cycle busy)
 *                 – MUL    ⇒ only segHalf² PP needed
 ******************************************************************************/
package klase32.include

import chisel3._
import chisel3.util._
import klase32.include.MultiplierHelpers.pad

/**
 * Parameter bundle for the non-pipelined multiplier FU.
 */
object SegmentSelType {
  sealed trait SegmentSel {
    def toOption: Option[Int]

    def asInt: Int = toOption.getOrElse(0)

    def >=(that: SegmentSel): Boolean = this match {
      case SegmentSel.Index(i) => that match {
        case SegmentSel.Index(i) => this.asInt >= that.asInt
        case SegmentSel.Default => true
      }
      case SegmentSel.Default => true
    }
  }



  object SegmentSel {
    case object Default extends SegmentSel {
      def toOption = None
    }

    case class Index(i: Int) extends SegmentSel {
      def toOption = Some(i)
    }

    implicit def fromInt(i: Int): SegmentSel = Index(i)
  }
}

object MulOp extends ChiselEnum { val MUL, MULH, MULHSU, MULHU = Value }

case class MulParams(
                      dataWidth:       Int = 32,
                      sliceWidth:      Int = 16,
                      slicesPerCycle:  Int = 1,
                      accumulationMethod: AccumMethod.Value = AccumMethod.IterativeAdder,
                      fusion:          Boolean = true,
                      earlyOutZero:    Boolean = false,
                      earlyOutOne:     Boolean = false,
                      earlyOutUpper16: Boolean = false,
                      vectorMode:      Boolean = false,
                      useDSP:          Boolean = false
                    ) {
  require(dataWidth % sliceWidth == 0,
    s"sliceWidth ($sliceWidth) must divide dataWidth ($dataWidth)")
  require(!earlyOutZero && !earlyOutOne)

  val segmentCount : Int = dataWidth / sliceWidth
  val guardBits    : Int = 32 - Integer.numberOfLeadingZeros(segmentCount - 1)
  val accWidth     : Int = dataWidth + guardBits
}

/** Enumeration of accumulation methods. */
object AccumMethod extends Enumeration {
  type AccumMethod = Value
  val IterativeAdder, CarrySaveTree, CarrySaveFusion, FlatArray = Value
}

/* ===============================================================
*  Utility helpers (guard‐bit + coordinate list)
* ============================================================ */
object MultiplierHelpers {
  def ceilDiv(a: Int, b: Int): Int = {require(a > 0 || b > 0); (a+b-1)/a}

  def pad(seg: UInt, signed: Bool, w: Int): UInt =
    Cat(Mux(signed, seg(w - 1), 0.U), seg)

}

class MultiplySlice(width: Int) extends Module {
  val io = IO(new Bundle {
    val en = Input(Bool())
    val a, b = Input(UInt((width + 1).W))
    val p = Output(UInt((2 * width + 2).W))
  })
  io.p := (io.a & Fill(width + 1, io.en) * io.b & Fill(width + 1, io.en))
}

class CoreIO(dataWidth: Int) extends Bundle {
  val packet = new Bundle {
    val a, b = Input(UInt(dataWidth.W))
    val op = Input(MulOp()) // 0:MUL 1:MULH 2:MULHSU 3:MULHU
  }
  val start  = Input(Bool())
  val kill   = Input(Bool())
  val busy   = Output(Bool())
  val done   = Output(Bool())
  val result = Output(UInt(dataWidth.W))
}

class IterativeAdderCore(parameters: MulParams) extends Module {
  // No 8 bit slice supported - only 32, 16
  // To support n bit - should handle complex matrix
  // No csa
  import MultiplierHelpers._
  import SegmentSelType.SegmentSel.Default
  import SegmentSelType.SegmentSel
  val io = IO(new CoreIO(parameters.dataWidth))

  private val segmentCount   = parameters.dataWidth / parameters.sliceWidth
  private val ppTotal    = segmentCount * segmentCount
  private val ppLow = segmentCount * (segmentCount + 1) / 2
  private val ppLowEarlyOut = (segmentCount / 2) * (segmentCount / 2)

  private val maxPhase = (ppTotal+parameters.slicesPerCycle-1)/parameters.slicesPerCycle
  private val phaseWidth = log2Ceil(maxPhase) max 1


  // returns shift amount and segment position of current indexing partial product
  val sliceIndexTable = Vector.tabulate(ppTotal) { index =>
    def shiftAmount(row: Int, col: Int) = (row + col) * parameters.sliceWidth
    def segmentPosition(row: Int, col: Int) = (row, col)

    val i = index / segmentCount
    val j = index % segmentCount
    (shiftAmount(i, j), segmentPosition(i, j))
  }

  // Only supports sliceWidth = 16, 32
  // Only supports slicePerCycle = 1, 2, 3
  require(Set(16, 32) contains parameters.sliceWidth)
  require(1 until 4 contains parameters.slicesPerCycle)
  require(ppTotal >= parameters.slicesPerCycle)

  // Hardware operation
  // Decode to uop
  sealed case class MulMicroOpcode(op: MulOp.Type, a: UInt, b: UInt) {
    def isLow: Bool = op === MulOp.MUL

    def aSigned = op === MulOp.MUL || op === MulOp.MULH || op === MulOp.MULHSU
    def bSigned = op === MulOp.MUL || op === MulOp.MULH

    def earlyOutOne = (a === (1.U) || b === (1.U)) && parameters.earlyOutOne.B
    def earlyOutZero = (a === (0.U) || b === (0.U)) && parameters.earlyOutZero.B
    def earlyOutUpper16 = (a(parameters.dataWidth - 1, parameters.dataWidth / 2) === (0.U) ||
      b(parameters.dataWidth - 1, parameters.dataWidth / 2) === (0.U)) && parameters.earlyOutUpper16.B

    def earlyOut = earlyOutOne || earlyOutZero || earlyOutUpper16
  }

  val uop = MulMicroOpcode(io.packet.op, io.packet.a, io.packet.b)

  val phaseTarget = Mux1H(Seq(
    (uop.isLow && uop.earlyOutUpper16)     -> ceilDiv(ppLowEarlyOut, parameters.slicesPerCycle).U,
    (uop.isLow && !uop.earlyOutUpper16)    -> ceilDiv(ppLow, parameters.slicesPerCycle).U,
    (!uop.isLow && uop.earlyOutUpper16)    -> 1.U,
    (!uop.isLow && !uop.earlyOutUpper16)   -> ceilDiv(ppTotal, parameters.slicesPerCycle).U,
    (uop.earlyOutOne || uop.earlyOutZero)  -> 1.U
  )
  )

  val slices = Seq.fill(parameters.slicesPerCycle)(Module(new MultiplySlice(parameters.sliceWidth)))
  //  slices.foreach { slice =>
  //    slice.io.en := false.B
  //    slice.io.a := 0.U
  //    slice.io.b := 0.U
  //  }

  val aSegment = VecInit(Seq.tabulate(segmentCount)(i => io.packet.a((i + 1) * parameters.sliceWidth - 1, i * parameters.sliceWidth)))
  val bSegment = VecInit(Seq.tabulate(segmentCount)(i => io.packet.b((i + 1) * parameters.sliceWidth - 1, i * parameters.sliceWidth)))
  // Vector of all partial products in single cycle
  val partialProductAligned = Wire(Vec(parameters.slicesPerCycle, UInt((2*parameters.dataWidth).W)))
  // (16, 16, 16, 16) * 1 ~ 3
  val partialProductSegment = VecInit(partialProductAligned.map { pp =>
    val segment = Wire(Vec(segmentCount * 2, UInt(parameters.sliceWidth.W)))
    segment.zipWithIndex.foreach { seg =>
      val index = seg._2
      seg._1 := pp((index + 1) * parameters.sliceWidth - 1, index * parameters.sliceWidth)
    }
    // (pp03, pp02, pp01, pp00)
    segment
  })
  val alignedSum = WireInit(0.U((2*parameters.dataWidth).W))

  /* ---------- State registers ---------- */
  // Phase = 0 -> start
  val accumulator   = RegInit(0.U(parameters.dataWidth.W))
  val phase = RegInit(0.U(phaseWidth.W))
  val running = RegInit(false.B)
  //  val phaseCur = WireInit(0.U(phaseWidth.W))
  //  val busyFlag = RegInit(false.B)


  /* ---------- Pre-compute slice products (fully constant fan-in) ---------- */
  // Conditions
  val w32n1 = parameters.sliceWidth == 32 && parameters.slicesPerCycle == 1
  val w16n1 = parameters.sliceWidth == 16 && parameters.slicesPerCycle == 1
  val w16n2 = parameters.sliceWidth == 16 && parameters.slicesPerCycle == 2
  val w16n3 = parameters.sliceWidth == 16 && parameters.slicesPerCycle == 3

  for (p <- 0 until maxPhase) {
    for (lane <- 0 until parameters.slicesPerCycle) {
      val idx = p * parameters.slicesPerCycle + lane
      if (idx < ppTotal) {
        val shiftAmount = sliceIndexTable(idx)._1
        val (aPos, bPos) = sliceIndexTable(idx)._2
        when(phase === p.U) {
          slices(lane).io.en := true.B
          slices(lane).io.a := pad(aSegment(aPos), uop.aSigned, parameters.sliceWidth)
          slices(lane).io.b := pad(bSegment(bPos), uop.bSigned, parameters.sliceWidth)
          partialProductAligned(lane) := slices(lane).io.p << shiftAmount
        }.otherwise {
          slices(lane).io.en := false.B
          slices(lane).io.a := 0.U
          slices(lane).io.b := 0.U
          partialProductAligned(lane) := 0.U
        }
      }
    }
  }

  // Assume alignedSum is always
  // add segments per phase
  // Default -> 0.U(16.W)
  val mulAddSegmentTable: Seq[(Int, Seq[(SegmentSel, SegmentSel)])] = {
    if (w16n1) Seq(0 -> Seq((Default, Default)), 1 -> Seq((1, 0)), 2 -> Seq((1, 0)), 3 -> Seq((Default, Default)))
    else if (w16n2) Seq(0 -> Seq((1, 0), (1, 0)), 1 -> Seq((1, 0), (Default, Default)))
    else if (w16n3) Seq(0 -> Seq((1, 0), (1, 0), (1, 0)), 1 -> Seq((Default, Default), (Default, Default), (Default, Default)))
    else Seq(0 -> Seq((Default, Default)))
  }

  // pass segments per phase
  // Includes initialize
  val mulPassSegmentTable: Seq[(Int, (SegmentSel, SegmentSel))] = {
    if (w16n1) Seq(0 -> (1, 0), 1 -> (1, 0), 2 -> (1, 0), 3 -> (Default, Default))
    else if (w16n2) Seq(0 -> (1, 0), 1 -> (Default, Default))
    else if (w16n3) Seq(0 -> (1, 0), 1 -> (Default, Default))
    else Seq(0 -> (Default, Default))
  }

  // MULH* is complex
  // Assume alignedSum is always
  // add segments per phase
  // Default -> 0.U(16.W)
  val mulhAddSegmentTable: Seq[(Int, Seq[(SegmentSel, SegmentSel)])] = {
    if (w16n1) Seq(0 -> Seq((Default, Default)), 1 -> Seq((2,1)), 2 -> Seq((2,1)), 3 -> Seq((3,2)))
    else if (w16n2) Seq(0 -> Seq((Default, 1), (2,1)), 1 -> Seq((2,1), (3,2)))
    else if (w16n3) Seq(0 -> Seq((Default, 1), (2,1), (2,1)), 1 -> Seq((Default, Default), (Default, Default), (3,2)))
    else Seq(0 -> Seq((Default, Default)))
  }

  // pass segments per phase
  // Includes initialize
  val mulhPassSegmentTable: Seq[(Int, (SegmentSel, SegmentSel))] = {
    if (w16n1) Seq(0 -> (Default, 1), 1 -> (2, 1), 2 -> (3, 2), 3 -> (Default, Default))
    else if (w16n2) Seq(0 -> (2, 1), 1 -> (Default, Default))
    else if (w16n3) Seq(0 -> (3, 2), 1 -> (Default, Default))
    else Seq(0 -> (Default, Default))
  }

  def valueOfSegment(seg: Vec[UInt], segIndex: SegmentSel): UInt = segIndex match {
    case SegmentSel.Index(i) => seg(i)
    case SegmentSel.Default => 0.U(parameters.sliceWidth.W)
  }

  def valueOfTwoSegments(seg: Vec[UInt], segIndexTableValue: (SegmentSel, SegmentSel)): UInt = {
    // returns high-low two segments
    require(segIndexTableValue._1 >= segIndexTableValue._2,
      s"Second element of segment table should not bigger than first one, " +
        s"($segIndexTableValue)")
    valueOfSegment(seg, segIndexTableValue._1) ## valueOfSegment(seg, segIndexTableValue._2)
  }

  def addSegmentsPerPhase(seg: Vec[UInt], addTable: Seq[(Int, Seq[(SegmentSel, SegmentSel)])], phase: Int): Seq[UInt] = {
    val tableMap = addTable.toMap
    tableMap(phase).map { segSel => valueOfTwoSegments(seg, segSel) }
  }

  def passSegmentsPerPhase(seg: Vec[UInt], passTable: Seq[(Int, (SegmentSel, SegmentSel))], phase: Int): UInt = {
    val tableMap = passTable.toMap
    valueOfTwoSegments(seg, tableMap(phase))
  }

  // Accumulate
  // alignedSum is result
  for (p <- 0 until maxPhase) {
    // By phase
    // Sum and Pass to next phase
    if (w32n1) {
      // no add: only 1 slice
      alignedSum := partialProductAligned(0)
    } else {
      // 16 bit slice case
      // MUL takes just lower 32 bits everytime
      when(phase === p.U) {
        when(uop.isLow) {
          // 32 bit add for acc and partial products(31, 0)
          // acc = sum(31, 0)
          alignedSum := accumulator(parameters.dataWidth - 1, 0) + addSegmentsPerPhase(partialProductSegment(p), mulAddSegmentTable, p).foldLeft(0.U(parameters.dataWidth.W))(_ + _)
          accumulator := passSegmentsPerPhase(partialProductSegment(p), mulPassSegmentTable, p)
        }.otherwise {
          // MULH
          alignedSum := accumulator(parameters.dataWidth - 1, 0) + addSegmentsPerPhase(partialProductSegment(p), mulhAddSegmentTable, p).foldLeft(0.U(parameters.dataWidth.W))(_ + _)
          accumulator := passSegmentsPerPhase(partialProductSegment(p), mulhPassSegmentTable, p)
        }
      }
    }
  }


  val lowerResult = Wire(UInt(parameters.dataWidth.W))
  val upperResult = Wire(UInt(parameters.dataWidth.W))

  lowerResult := alignedSum(parameters.dataWidth-1, 0)
  upperResult := alignedSum(parameters.dataWidth*2-1, parameters.dataWidth)

  /* ---------- Start / kill handling ---------- */
  //  val oneCycleCond = phaseTarget === 0.U
  //  io.busy := phase =/= phaseTarget
  //  io.done := phase === phaseTarget
  val done = WireDefault(false.B)

  when(io.start) {
    phase := 0.U
    running := phaseTarget =/= 0.U
    //    dataReg := io.packet
    when(phaseTarget === 0.U) {
      done := true.B
    }
  }.elsewhen(running) {
    val next = phase + 1.U
    phase := next
    when(next === phaseTarget) {
      done := true.B
      running := false.B
    }
  }

  io.busy := running
  io.done := done
  //
  //  when(io.start && oneCycleCond) {
  //    phaseCur := 0.U
  //    io.busy := false.B
  //    io.done := true.B
  //  }.elsewhen(io.start && !oneCycleCond) {
  //    phaseCur := 0.U
  //    io.busy := true.B
  //    io.done := false.B
  //  }.elsewhen(io.kill) {
  //    phaseCur := 0.U
  //    io.busy := false.B
  //    io.done := false.B
  //  }.elsewhen(busyFlag) {
  //    phaseCur := phase + 1.U
  //    io.busy := phaseCur =/= phaseTarget
  //    io.done := phase === phaseTarget
  //  }.otherwise {
  //    phaseCur := 0.U
  //    io.busy := false.B
  //    io.done := false.B
  //  }

  //  phase := phaseCur
  //  busyFlag := busy

  /* ---------- Outputs ---------- */
  io.result := Mux(uop.isLow, lowerResult, upperResult)
}

/*
/* ===============================================================
* 2. CarrySaveTreeCore  – CSA tree + same-cycle CPA
* ============================================================ */
class CarrySaveTreeCore(parameters: MulParams) extends Module {
  import MultiplierHelpers._
  val io = IO(new CoreIO(parameters.dataWidth))
  private val segmentCount   = parameters.dataWidth / parameters.sliceWidth
  private val guardBitCount  = guardBits(segmentCount)
  private val accumulatorWidth = parameters.dataWidth + guardBitCount

  def signSlice(x: UInt, signed: Boolean): SInt = Cat(Mux(signed.B, x(parameters.sliceWidth - 1), 0.U), x).asSInt

  val partials = for {
    i <- 0 until segmentCount
    j <- 0 until segmentCount
  } yield {
    val sliceA = io.a((i + 1) * parameters.sliceWidth - 1, i * parameters.sliceWidth)
    val sliceB = io.b((j + 1) * parameters.sliceWidth - 1, j * parameters.sliceWidth)
    val aSigned = io.op =/= 3.U
    val bSigned = (io.op === 0.U) || (io.op === 1.U)
    val rawProd = (signSlice(sliceA, aSigned) * signSlice(sliceB, bSigned)).asUInt(accumulatorWidth.W)
    val shifted = if (i + j < segmentCount) rawProd else (rawProd >> parameters.dataWidth)(accumulatorWidth - 1, 0)
    val useLo   = i + j < segmentCount
    if ((io.op === 0.U && useLo) || (io.op =/= 0.U && !useLo)) shifted else 0.U(accumulatorWidth.W)
  }

  // CSA reduction
  def reduceToTwoRows(inputs: Seq[UInt]): Seq[UInt] =
    if (inputs.length <= 2) inputs
    else reduceToTwoRows(inputs.grouped(3).flatMap {
      case Seq(x, y, z) => Seq(x ^ y ^ z, ((x & y) | (x & z) | (y & z)) << 1)
      case rest         => rest
    }.toSeq)

  val twoRows = reduceToTwoRows(partials)
  val resultFull = twoRows.reduce(_ +& _)

  io.busy  := io.start && !io.kill
  io.done  := !io.busy
  val lo = resultFull(parameters.dataWidth - 1, 0)
  val hi = resultFull(accumulatorWidth - 1, guardBitCount)
  io.result := Mux(io.op === 0.U, lo, hi)
}

/* ===============================================================
* 3. CarrySaveFusionCore  – CSA tree, CPA next cycle (latency-1)
* ============================================================ */
class CarrySaveFusionCore(parameters: MulParams) extends Module {
  import MultiplierHelpers._
  val io = IO(new CoreIO(parameters.dataWidth))
  private val segmentCount = parameters.dataWidth / parameters.sliceWidth
  private val guardBitCount = guardBits(segmentCount)
  private val accumulatorWidth = parameters.dataWidth + guardBitCount

  def signSlice(x: UInt, signed: Boolean): SInt = Cat(Mux(signed.B, x(parameters.sliceWidth - 1), 0.U), x).asSInt

  val partials = for {
    i <- 0 until segmentCount
    j <- 0 until segmentCount
  } yield {
    val sliceA = io.a((

*/
