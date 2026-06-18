lazy val scala3Version = "3.8.2"
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

    // Optional: falls du Klassen im Default Package hast und sie ausschließen willst.
    // Wenn dadurch zu viel ausgeschlossen wird, diese Zeile entfernen.
    coverageExcludedPackages := "<empty>.*",

    // Schließt Main.scala und src/main/scala/view/Gui.scala aus der Coverage aus.
    // Wichtig: ohne ".scala" am Ende.
    coverageExcludedFiles := ".*[\\\\/]Main;.*[\\\\/]view[\\\\/]Gui",

    // Schließt GuiTest.scala komplett aus:
    // wird nicht kompiliert und nicht ausgeführt.
    Test / unmanagedSources := (Test / unmanagedSources).value.filterNot { file =>
      file.getName == "GuiTest.scala"
    }
  )
