lazy val scala3Version = "3.8.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "cubatro",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,

    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.14",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14" % Test,
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0",

    // Wichtig: Regex gegen Dateipfad OHNE ".scala".
    // Simpel halten, damit es sicher matcht.
    coverageExcludedFiles := ".*Gui;.*Main",

    // GuiTest.scala gar nicht kompilieren und nicht ausführen
    Test / unmanagedSources := (Test / unmanagedSources).value.filterNot { file =>
      file.getName == "GuiTest.scala"
    },

    // Optional, aber sinnvoll, wenn CI bei <100% fehlschlagen soll
    coverageFailOnMinimum := true,
    coverageMinimumStmtTotal := 100,
    coverageMinimumBranchTotal := 100
  )