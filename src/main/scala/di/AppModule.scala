package di

import controller.{GameController, IController}
import fileio.{FileIO, JsonFileIO}
import view.{Gui, IView, Tui}

trait AppModule:
  def fileIO: FileIO
  def controller: IController
  def tui(using IController): IView
  def startGui(using IController): Unit

class DefaultAppModule(
    guiLauncher: IController => Unit = Gui.launcher,
    fileIOProvider: () => FileIO = () => new JsonFileIO()
) extends AppModule:

  override val fileIO: FileIO =
    fileIOProvider()

  override val controller: IController =
    new GameController(fileIO = fileIO)

  override def tui(using controller: IController): IView =
    new Tui(controller)

  override def startGui(using controller: IController): Unit =
    guiLauncher(controller)

final class AppInjector(module: AppModule):
  val fileIO: FileIO = module.fileIO
  val controller: IController = module.controller
  val tui: IView = module.tui(using controller)

  def startGui(): Unit =
    module.startGui(using controller)

object AppInjector:
  def from(module: AppModule): AppInjector =
    new AppInjector(module)
