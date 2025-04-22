package klase32

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import klase32.include.config._
import klase32.include.param.KLASE32ParamKey
import klase32.include.KLASE32AbstractClass._
import klase32.include.ControlSignal._
import klase32.include.util.IntDivWithRound.IntDivWithRoundOps
// import klase32.interface._

// dataBits: 32 * n
// sliceBits: 8, 16, 32, ...
// nSlice: 1, 2, ... (# 1-cycle multiplier slice)
// enableFast: For upper half == 0 -> fast MUL/MULH
// enableFusion: MUL -> MULH 1-cycle hit

object MulOp extends ChiselEnum {val MUL, MULH, MULHSU, MULHU = Value}

// 1-cycle w-bit x w-bit signed multiplier
class Slice(w: Int) extends Module {
  val io = IO(new Bundle {
    val en = Input(Bool())
    val a, b = Input(UInt((w + 1).W))
    val p = Output(UInt((2 * w + 2).W))
  })
  val reg = Reg(UInt((2 * w + 2).W))
  when (io.en) { reg := (io.a.asSInt * io.b.asSInt).asUInt}
  io.p := reg
}

class UniMulCarryCut(
                    dataBits: Int = 32,
                    sliceBits: Int = 16,
                    nSlice: Int = 3,
                    enableFast: Boolean = true,
                    enableFusion: Boolean = true
                    ) extends Module {
  // Check parameters
  require(dataBits % 32 == 0 && dataBits >= 32)
  require(dataBits % sliceBits == 0 && sliceBits >= 8)
  require(sliceBits >= 1)

  // Constants
  private val seg = dataBits / sliceBits  // 2, 4, 8 ...
  private val segHalf = seg / 2           // # lower half segments
  private val ppCnt = seg * seg           // # total partial product
  private val lowPP = segHalf * segHalf + (if (segHalf > 1) 2 * segHalf * (segHalf - 1) else 0) // # lower half calculation partial product, cross
  private val cCarry = log2Ceil(nSlice)   // accumulator carry width
  private val keepW = sliceBits + cCarry  // minimal accumulator width

  // IO
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Bundle {
      val rs1, rs2 = UInt(5.W)
      val a, b = UInt(dataBits.W)
      val op = MulOp()
    }))
    val out = Decoupled(UInt(dataBits.W))
  })

  // Pad function for slice input
  def pad(opSigned: Bool, seg: UInt): UInt = {
    Cat(Mux(opSigned, seg(sliceBits - 1), 0.U), seg)
  }

  // Fast path for upper half == 0
  def zeroHalf(x: UInt) = x(dataBits-1, dataBits/2) === 0.U
  val fastCond = enableFast.B && zeroHalf(io.in.bits.a) && zeroHalf(io.in.bits.b)
  val lowPPfast = segHalf * segHalf // Only for partial product ALBL
  val latFast = ((lowPPfast + nSlice - 1) / nSlice).U // fast MUL latency

  // Fusion
  val lastRS1 = Reg(UInt(5.W))
  val lastRS2 = Reg(UInt(5.W))
  val lastHi  = Reg(UInt(dataBits.W))
  val lastVal = RegInit(false.B)

  val fusionHit = enableFusion.B &&
    io.in.valid &&
    (io.in.bits.op === MulOp.MULH) &&
    lastVal &&
    (io.in.bits.rs1 === lastRS1) &&
    (io.in.bits.rs2 === lastRS2)

  // State
  val busy = RegInit(false.B)
  val phase = RegInit(0.U(6.W)) // <- why?
  val req = Reg(io.in.bits.cloneType)

  io.in.ready := (!busy || fusionHit) || (fastCond && io.in.fire)
  when(io.in.fire && !fusionHit) {
    busy := true.B
    phase := 0.U
    req := io.in.bits
  }.elsewhen(busy) {
    phase := phase + 1.U
  }

  // Multiplier Slice
  val slices = Seq.fill(nSlice)(Module(new Slice(sliceBits)))
  slices.foreach(_.io.en := false.B)

  val rcMap = (for (r <- 0 until seg; c <- 0 until seg) yield (r, c)).toArray
  val ptr = phase * nSlice.U  // first index this cycle

  // Accumulator
  val keepR = Reg(UInt(keepW.W))
  val lowTmp = Wire(UInt(sliceBits.W))
  lowTmp := 0.U

  // Schedule & add
  when(ptr < ppCnt.U) {
    // ??
    //var addLow = 0.U(sliceBits.W)
    //var addKeep = keepR
    val addLow = WireInit(0.U(sliceBits.W))
    val addKeep = keepR

    for (k <- 0 until nSlice) {
      val idx = ptr + k.U
      when(idx < ppCnt.U) {
        val (r, c) = rcMap(idx.litValue.toInt)
        slices(k).io.en := true.B
        slices(k).io.a := Cat(req.bits.a(sliceBits * r + sliceBits - 1), req.bits.a(sliceBits * (r + 1) - 1, sliceBits * r))
        slices(k).io.b := Cat(req.bits.b(sliceBits * c + sliceBits - 1), req.bits.b(sliceBits * (c + 1) - 1, sliceBits * c))

        val pp = slices(k).io.p
        val up = pp(sliceBits * 2 - 1, sliceBits)
        val lo = pp(sliceBits - 1, 0)
        addLow = addLow + lo
        addKeep = addKeep + up
      }
    }
    val sumLow = addLow +& lowTmp
    val carryLo = sumLow(sliceBits, sliceBits - cCarry + 1)
    lowTmp := sumLow(sliceBits - 1, 0)
    keepR := addKeep + carryLo
  }

  when(ptr + nSlice.U >= ppCnt.U) {busy := false.B}

  // Cacluate Latency
  val latLow = ((lowPP + nSlice - 1) / nSlice).U
  val doneLowNormal = (!busy && phase === latLow - 1.U)
  val doneLowFast = fastCond && (phase === latFast - 1.U)

  val doneLow = doneLowNormal || doneLowFast
  val doneHigh = (!busy && phase === ((ppCnt + nSlice - 1) / nSlice).U - 1.U) ||
    (fastCond && phase === 0.U)

  // Result
  val fastLow = (req.bits.a(sliceBits - 1, 0) * req.bits.b(sliceBits - 1, 0))(dataBits - 1, 0)
  io.out.valid := Mux(io.in.bits.op === MulOp.MUL, doneLow || fusionHit, doneHigh)
  io.out.bits := MuxCase(0.U, Array(
    fusionHit -> lastHi,
    doneLowFast -> fastLow,
    doneLowNormal -> lowTmp.pad(dataBits),
    (io.in.bits.op === MulOp.MULH && fastCond) -> 0.U,
    (io.in.bits.op === MulOp.MULH) -> keepR(dataBits - 1, 0),
  ))

  // Update fusion buffer
  when(enableFusion.B && doneLow && io.out.fire && io.in.bits.op === MulOp.MUL) {
    lastRS1 := req.bits.rs1
    lastRS2 := req.bits.rs2
    lastHi := keepR(dataBits - 1, 0)
    lastVal := true.B
  }.elsewhen(fusionHit) {
    false.B
  }
}

class MPY(implicit p: Parameters) extends CoreModule {

  import MPYControlIE._

  val k = p(KLASE32ParamKey)

  val io = IO(new Bundle {
    val ctrl = Input(MPYControlIE())
    val A = Input(SInt(xLen.W))
    val B = Input(SInt(xLen.W))
    val fusedMul = Input(Bool())
    val prod = Output(UInt(xLen.W))

    val busy = Output(Bool())
  }
  )

  val rs1Signed = Wire(Bool())
  val rs2Signed = Wire(Bool())
  val mostSignificantWord = Wire(Bool())

  when(io.ctrl === MUL) {
    rs1Signed := true.B
    rs2Signed := true.B
    mostSignificantWord := false.B
  }.elsewhen(io.ctrl === MULH) {
    rs1Signed := true.B
    rs2Signed := true.B
    mostSignificantWord := true.B
  }.elsewhen(io.ctrl === MULHU) {
    rs1Signed := false.B
    rs2Signed := false.B
    mostSignificantWord := true.B
  }.elsewhen(io.ctrl === MULHSU) {
    rs1Signed := true.B
    rs2Signed := false.B
    mostSignificantWord := true.B
  }.otherwise {
    rs1Signed := false.B
    rs2Signed := false.B
    mostSignificantWord := false.B
  }
  val a = Wire(SInt((xLen + 1).W))
  val b = Wire(SInt((xLen + 1).W))

  when(io.ctrl =/= default) {
    a := Cat((rs1Signed && io.A(xLen - 1)), io.A.asUInt).asSInt
    b := Cat((rs2Signed && io.B(xLen - 1)), io.B.asUInt).asSInt
  }.otherwise {
    a := 0.S
    b := 0.S
  }

  val prod = Wire(UInt(xLen.W))

  if (mpyConfig == 4) {
    val prodRaw = Wire(SInt(((xLen + 1) * 2 + 1).W))
    prodRaw := a * b
    prod := Mux(mostSignificantWord, prodRaw(63, 32).asSInt, prodRaw(31, 0).asSInt).asUInt
    io.prod := prod
    io.busy := false.B
  } else {
    val mulBitWidth = xLen / 2
    val numChunks = xLen/mulBitWidth

    val operandUpper16isZero = (io.A(xLen - 1, xLen / 2) === 0.U) && (io.B(xLen - 1, xLen / 2) === 0.U)
    val accum = Reg(UInt((xLen + 2).W))

    val aChunks = Wire(Vec(numChunks, SInt((mulBitWidth+1).W)))
    val bChunks = Wire(Vec(numChunks, SInt((mulBitWidth+1).W)))

    val mulInput = Seq(al, ah).flatMap(ai => Seq(bl, bh).map(bi => (ai, bi)))

    if (mpyConfig == 3) {
      val numMul = 3

      val mulOpA = Wire(Vec(numMul, SInt()))
      val mulOpB = Wire(Vec(numMul, SInt()))
      when(!mostSignificantWord) {
        (mulOpA zip mulOpB).foreach { case (a, b) =>
          a := al
          b := bl
        }

      }

    }
  }
}
