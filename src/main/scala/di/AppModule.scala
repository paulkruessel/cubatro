package di

import com.google.inject.{AbstractModule, Guice, Injector, Module, Singleton}
import controller.{GameController, IController}
import fileio.{FileIO, JsonFileIO}
import view.{Gui, IView, Tui}

trait GuiLauncher:
  def start(controller: IController): Unit

final class SwingGuiLauncher(launcher: IController => Unit = Gui.launcher) extends GuiLauncher:
  override def start(controller: IController): Unit =
    launcher(controller)

class DefaultAppModule(
    fileIOImplementation: Class[? <: FileIO] = classOf[JsonFileIO],
    guiLauncher: GuiLauncher = new SwingGuiLauncher()
) extends AbstractModule:

  override def configure(): Unit =
    bind(classOf[FileIO]).to(fileIOImplementation).in(classOf[Singleton])
    bind(classOf[IController]).to(classOf[GameController]).in(classOf[Singleton])
    bind(classOf[IView]).to(classOf[Tui]).in(classOf[Singleton])
    bind(classOf[GuiLauncher]).toInstance(guiLauncher)

final class AppInjector private (injector: Injector):
  val fileIO: FileIO = injector.getInstance(classOf[FileIO])
  val controller: IController = injector.getInstance(classOf[IController])
  val tui: IView = injector.getInstance(classOf[IView])

  def startGui(): Unit =
    injector.getInstance(classOf[GuiLauncher]).start(controller)

object AppInjector:
  def create(fileIOImplementation: Class[? <: FileIO] = classOf[JsonFileIO]): AppInjector =
    from(new DefaultAppModule(fileIOImplementation = fileIOImplementation))

  def from(module: Module): AppInjector =
    new AppInjector(Guice.createInjector(module))
