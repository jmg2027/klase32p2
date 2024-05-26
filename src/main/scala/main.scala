// package klas

// import chisel3._
// import org.chipsalliance.cde.config.{Config, Parameters}

// import klase32.KLASE32
// import klastools.BaseGenerator
// // import klase32.config.{Config, Parameters}
// import klase32.param.DefaultConfig

// object main extends App { new Generator(args) }
// class Generator(args: Array[String]) extends BaseGenerator(args) {
//   implicit override lazy val config: Config = configName match {
//     case "empty" => new Config(Parameters.empty)
//     case "arb" => new Config(Parameters.empty)
//     case _ => {
//       throw new RuntimeException(s"Invalid config name. THere is no \"${configName}\" config")
//     }
//   }

//   override lazy val harness: Module = designName match {
//     case "klase32p2" => new KLASE32(0)(new DefaultConfig)
//     case _ => {
//       throw new RuntimeException(s"Invalid design name. There is no \"${designName}\" design")
//     }
//   }

//   generate
// }
