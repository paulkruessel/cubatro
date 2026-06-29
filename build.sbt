import sbtassembly.AssemblyPlugin.autoImport._
import sbtassembly.{MergeStrategy, PathList}

lazy val scala3Version = "3.8.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "cubatro",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,

    Compile / mainClass := Some("main"),
    assembly / mainClass := Some("main"),
    assembly / assemblyJarName := "cubatro-assembly.jar",
    assembly / test := {},
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", "MANIFEST.MF") =>
        MergeStrategy.discard

      case PathList("META-INF", xs @ _*)
          if xs.nonEmpty && (
            xs.last.endsWith(".SF") ||
            xs.last.endsWith(".DSA") ||
            xs.last.endsWith(".RSA")
          ) =>
        MergeStrategy.discard

      case PathList("module-info.class") =>
        MergeStrategy.discard

      case PathList("META-INF", "versions", "9", "module-info.class") =>
        MergeStrategy.discard

      case x =>
        val oldStrategy = (assembly / assemblyMergeStrategy).value
        oldStrategy(x)
    },

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