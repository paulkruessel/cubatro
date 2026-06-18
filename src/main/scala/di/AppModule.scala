package di

import controller.{GameController, IController}
import view.{Gui, IView, Tui}

trait AppModule:
  def controller: IController
  def tui(using IController): IView
  def startGui(using IController): Unit

class DefaultAppModule(
    guiLauncher: IController => Unit = Gui.launcher
) extends AppModule:
  override val controller: IController = new GameController()

  override def tui(using controller: IController): IView =
    new Tui(controller)

  override def startGui(using controller: IController): Unit =
    guiLauncher(controller)

final class AppInjector(module: AppModule):
  val controller: IController = module.controller
  val tui: IView = module.tui(using controller)

  def startGui(): Unit =
    module.startGui(using controller)

object AppInjector:
  def from(module: AppModule): AppInjector =
    new AppInjector(module)
