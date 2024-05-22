package klase32

import chisel3._
import chiseltest._
import klas.KlasTest
import klase32.config._
import klase32.param.DefaultConfig

class FrontendTest extends KlasTest {
  implicit val p = new DefaultConfig
  behavior of "Frontend"

  it should "correctly handle control flows and fetch operations" in {
    test(new Frontend) { c =>
      // Function to simulate a single clock cycle of operation
      def simulateCycle(ctrlSignal: FrontendControlIE.Type, pc: BigInt, evec: BigInt, cnd: Boolean, exception: Boolean, eret: Boolean, stall: Boolean, divBusy: Boolean, aluR: BigInt): Unit = {
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
        println(c.io.issue.peekBoolean())
        println(c.io.pcRegWrite.valid.peekBoolean())
        println(c.io.pcRegWrite.bits.peekInt())
        println(c.io.epm.elements.map {
          case (k, v) =>
            k -> v.peek()
//            v match {
//            case _: UInt => v.asUInt.peek()
//            case _: Bool => v.asBool.peek()
//            case _ => v
//          }
        }
        )
      }

      // Initial Conditions
      c.reset.poke(true.B)
      simulateCycle(FrontendControlIE.default, 0x1000, 0x0, cnd = false, exception = false, eret = false, stall = false, divBusy = false, 0x0)
      c.reset.poke(false.B)

      // Test basic fetch
      simulateCycle(FrontendControlIE.default, 0x1000, 0x0, cnd = false, exception = false, eret = false, stall = false, divBusy = false, 0x0)

      // Test branching
      simulateCycle(FrontendControlIE.BR, 0x1000, 0x0, cnd = true, exception = false, eret = false, stall = false, divBusy = false, 0x1040)

      // Test stall conditions
      simulateCycle(FrontendControlIE.default, 0x1040, 0x0, cnd = false, exception = false, eret = false, stall = true, divBusy = false, 0x1040)

      // Test exception handling
      simulateCycle(FrontendControlIE.default, 0x1040, 0x2000, cnd = false, exception = true, eret = false, stall = false, divBusy = false, 0x1040)

      // Additional scenarios to cover jumps, eret, flushes, and interrupts
    }
  }
}
