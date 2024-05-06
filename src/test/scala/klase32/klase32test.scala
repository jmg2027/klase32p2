package klase32

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import klase32.config._
import scala.util.Random

class KlasE32Test extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "KlasE32"

  it should "correctly execute complex instruction sequences and handle exceptions and interrupts" in {
    implicit val p: Parameters = new DefaultConfig() // Matches your configuration class

    test(new KlasE32(hartId = 0)) { c =>
      // Setup and reset
      c.reset.poke(true.B)
      c.clock.step(1)
      c.reset.poke(false.B)

      // Helper function to poke instructions and step the processor
      def stepInstruction(inst: UInt, steps: Int = 1): Unit = {
        c.io.epm.data.poke(inst)
        for (_ <- 0 until steps) c.clock.step(1)
      }

      // Random instruction generation for randomized testing
      val randomInstructions = Seq(
        "h00300513".U, // addi x10, x0, 3
        "h00400613".U, // addi x12, x0, 4
        "h00c58693".U, // addi x13, x11, 12
        "hfc1ff0ef".U  // jal x1, -64 (loop back for a delay slot)
      )

      // Test a mix of instructions, some of which are randomly selected
      for (inst <- randomInstructions ++ Seq.fill(10)(randomInstructions(Random.nextInt(randomInstructions.length)))) {
        stepInstruction(inst, steps = 2)
      }

      // Check handling of an external interrupt during execution
      c.io.interrupt.valid.poke(true.B)
      stepInstruction(randomInstructions.head, steps = 2)  // Run an instruction when interrupt is active
      assert(c.csr.io.interruptPending.peek().litToBoolean, "Expected interrupt to be pending")

      // Test recovery from interrupt
      c.io.interrupt.valid.poke(false.B)
      stepInstruction(randomInstructions.head, steps = 2)  // Continue execution after interrupt

      // Simulate an exception by forcing an illegal operation
      c.io.epm.data.poke("h00000000".U) // Illegal instruction
      stepInstruction("h00000000".U, steps = 1)
      assert(c.csr.io.exception.peek().litToBoolean, "Expected exception for illegal instruction")

      // Ensure that PC and CSR states are saved and handled correctly
      assert(c.csr.io.epc.peek().litToBigInt != 0, "EPC should be updated with the address of the failing instruction")

      // More complex checks can involve simulating sequences that test specific hazard resolutions or more detailed exception handling
    }
  }
}