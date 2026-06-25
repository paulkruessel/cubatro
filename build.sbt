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
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "2.4.0",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
    libraryDependencies += "com.lihaoyi" %% "upickle" % "4.3.2",
    libraryDependencies += "com.google.inject" % "guice" % "7.0.0",

    coverageExcludedPackages := "<empty>.*",
    coverageExcludedFiles := ".*[\\\\/]Main;.*[\\\\/]view[\\\\/]Gui",

    Test / unmanagedSources := (Test / unmanagedSources).value.filterNot { file =>
      file.getName == "GuiTest.scala"
    }
  )
