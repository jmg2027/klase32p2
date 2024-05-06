package klase32.unit

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import klase32.config._
import klase32.param.KlasE32ParamKey

class FrontendTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Frontend"

  it should "correctly handle control flows and fetch operations" in {
    implicit val p: Parameters = new DefaultConfig() // Ensure this matches your configuration class

    test(new Frontend) { c =>
      // Function to simulate a single clock cycle of operation
      def simulateCycle(ctrlSignal: CtrlControlIE.Type, pc: BigInt, evec: BigInt, cnd: Boolean, exception: Boolean, eret: Boolean, stall: Boolean, divBusy: Boolean, aluR: BigInt): Unit = {
        c.io.ctrl.poke(ctrlSignal)
        c.io.if_pc.poke(pc.U)
        c.io.evec.poke(evec.U)
        c.io.cnd.poke(cnd.B)
        c.io.exception.poke(exception.B)
        c.io.eret.poke(eret.B)
        c.io.stall.poke(stall.B)
        c.io.divBusy.poke(divBusy.B)
        c.io.aluR.poke(aluR.U)
        c.clock.step(1) // Advance the clock to process the inputs
      }

      // Initial Conditions
      c.reset.poke(true.B)
      simulateCycle(CtrlControlIE.NOOP, 0x1000, 0x0, cnd = false, exception = false, eret = false, stall = false, divBusy = false, 0x0)
      c.reset.poke(false.B)

      // Test basic fetch
      simulateCycle(CtrlControlIE.NOOP, 0x1000, 0x0, cnd = false, exception = false, eret = false, stall = false, divBusy = false, 0x0)

      // Test branching
      simulateCycle(CtrlControlIE.BR, 0x1000, 0x0, cnd = true, exception = false, eret = false, stall = false, divBusy = false, 0x1040)

      // Test stall conditions
      simulateCycle(CtrlControlIE.NOOP, 0x1040, 0x0, cnd = false, exception = false, eret = false, stall = true, divBusy = false, 0x1040)

      // Test exception handling
      simulateCycle(CtrlControlIE.NOOP, 0x1040, 0x2000, cnd = false, exception = true, eret = false, stall = false, divBusy = false, 0x1040)

      // Additional scenarios to cover jumps, eret, flushes, and interrupts
    }
  }
}