import di.AppInjector
import fileio.{FileIO, JsonFileIO, XmlFileIO}

import java.awt.GraphicsEnvironment

@main def main(): Unit =
  runApp(createInjector = () => AppInjector.create(configuredFileIOImplementation()))

def configuredFileIOImplementation(
    properties: Map[String, String] = sys.props.toMap,
    environment: Map[String, String] = sys.env
): Class[? <: FileIO] =
  val configuredFormat =
    properties
      .get("cubatro.fileio")
      .orElse(environment.get("CUBATRO_FILEIO"))
      .orElse(environment.get("CUBATRO_FILE_IO"))
      .getOrElse("json")

  fileIOImplementationFor(configuredFormat)

def fileIOImplementationFor(format: String): Class[? <: FileIO] =
  Option(format).map(_.trim.toLowerCase).getOrElse("json") match
    case "json" => classOf[JsonFileIO]
    case "xml"  => classOf[XmlFileIO]
    case other =>
      throw new IllegalArgumentException(s"Unsupported FileIO format '$other'. Use 'json' or 'xml'.")

def runApp(
    isHeadless: Boolean = GraphicsEnvironment.isHeadless(),
    createInjector: () => AppInjector = () => AppInjector.create()
): Unit =
  val injector = createInjector()
  val controller = injector.controller
  val tui = injector.tui

  if !isHeadless then
    injector.startGui()
  else
    println("GUI not started: no graphical display available.")

  controller.start()

  tui.run()
