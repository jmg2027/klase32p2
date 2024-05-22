package klase32

import chisel3._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import klase32.config.Parameters
import klase32.param.{DefaultConfig, KlasE32ParamKey}

object Main extends App {
  implicit val p: Parameters = (new DefaultConfig).toInstance
  (new ChiselStage).execute(args, Seq(ChiselGeneratorAnnotation(() => new KLASE32(0))))
}