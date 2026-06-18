val scala3Version = "3.8.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "cubatro",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,

    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.14",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14" % Test,
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0",

    Test / testOptions += Tests.Filter(testName =>
      !testName.endsWith("MainTest") && !testName.endsWith("GuiTest")
    ),
    coverageExcludedPackages := "<empty>.*",
    coverageExcludedFiles := ".*Main.scala;.*Gui.scala",
    coverageFailOnMinimum := true,
    coverageMinimumStmtTotal := 100,
    coverageMinimumBranchTotal := 100
  )
