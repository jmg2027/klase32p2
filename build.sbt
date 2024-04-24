// See README.md for license details.

ThisBuild / scalaVersion     := "2.13.10"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "com.github.jmg2027"

val chiselVersion = "3.6.0"

val rocketChipDir = file("generators/rocket-chip")


lazy val root = (project in file("."))
  .dependsOn(rocket_chip)
  .settings(commonSettings)
  

lazy val commonSettings = Seq(
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % chiselVersion,
      "org.scalatest" %% "scalatest" % "3.2.16" % "test",
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
      "-Ymacro-annotations",
    ),
    addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full),
)

lazy val cde = (project in rocketChipDir / "cde")
  .settings(
    commonSettings,
    Compile / scalaSource := baseDirectory.value / "cde/src/chipsalliance/rocketchip"
  )


lazy val rocket_macros  = (project in rocketChipDir / "macros")
  .settings(
    commonSettings,
    libraryDependencies ++= Seq(
      "org.json4s" %% "json4s-jackson" % "4.0.6",
    )
  )

lazy val ucb_hardfloat = (project in rocketChipDir / "hardfloat")
  .settings(commonSettings)


lazy val rocket_chip = (project in rocketChipDir)
  .dependsOn(cde, rocket_macros, ucb_hardfloat)
  .settings(commonSettings)
