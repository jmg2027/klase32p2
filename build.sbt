// See README.md for license details.
import scala.sys.process._

// ThisBuild / scalaVersion     := "2.13.10"
// ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "com.github.jmg2027"
//-------------------------------------------------
// Global setting
//-------------------------------------------------
val scalaTestVersion = "3.2.0"
val chiselVersion = "3.6.0"
val chiselTestVersion = "0.6.0"

lazy val chiselSettings = Seq(
  libraryDependencies ++= Seq("edu.berkeley.cs" %% "chisel3" % chiselVersion),
  addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full)
)

lazy val chiselTestSetting = Seq(
  libraryDependencies ++= Seq("edu.berkeley.cs" %% "chiseltest" % chiselTestVersion % "test"))

val rocketChipDir = file("generators/rocket-chip")

def freshProject(name: String, dir: File): Project = {
  Project(id = name, base = dir / "src")
    .settings(
      Compile / scalaSource := baseDirectory.value / "main" / "scala",
      Compile / resourceDirectory := baseDirectory.value / "main" / "resources"
    )
}

lazy val rocketSettings = Seq(
  scalaVersion := "2.13.10",
  scalacOptions ++= Seq("-deprecation", "-unchecked"),
  libraryDependencies ++= Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value),
  libraryDependencies ++= Seq("org.json4s" %% "json4s-native" % "4.0.6"),
  libraryDependencies ++= Seq("org.json4s" %% "json4s-jackson" % "4.0.6"),
  libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % scalaTestVersion % "test"),
  libraryDependencies ++= Seq("com.typesafe.play" %% "play-json" % "2.9.2"),
  libraryDependencies ++= Seq("com.lihaoyi" %% "mainargs" % "0.5.0"),
  // libraryDependencies ++= Seq(
  //   "org.chipsalliance.cde" %% "cde" % "1.0.0"
  // )
  allDependencies := {
    // drop specific maven dependencies in subprojects
    val dropDeps = Seq(("edu.berkeley.cs", "rocketchip"))
    allDependencies.value.filterNot { dep =>
      dropDeps.contains((dep.organization, dep.name))
    }
  },
  exportJars := true,
)

lazy val commonSettings = rocketSettings ++ Seq(
  unmanagedBase := (rocketchip / unmanagedBase).value,
)
//-------------------------------------------------
// ~Global setting
//-------------------------------------------------
//-------------------------------------------------
// Rocket-chip dependencies
// val rocketChipDir = file("design/thirdparty/rocket-chip")

lazy val cde = (project in rocketChipDir / "cde")
  .settings(rocketSettings)
  .settings(Compile / scalaSource := baseDirectory.value / "cde/src/chipsalliance/rocketchip")
  .settings(publishArtifact := false)

lazy val hardfloat  = (project in rocketChipDir / "hardfloat")
  .settings(rocketSettings, chiselSettings)
  .settings(publishArtifact := false)

lazy val rocketMacros  = (project in rocketChipDir / "macros")
  .settings(rocketSettings, chiselSettings)
  .settings(publishArtifact := false)
    

lazy val rocketchip = freshProject("rocketchip", rocketChipDir)
  .dependsOn(hardfloat, rocketMacros, cde)
  .settings(rocketSettings, chiselSettings)

lazy val rocketLibDeps = (rocketchip / Keys.libraryDependencies)

lazy val root = (project in file("."))
  .dependsOn(rocketchip)
  // .settings(libraryDependencies ++= rocketLibDeps.value)
  .settings(chiselSettings, commonSettings)
  .settings(chiselTestSetting)