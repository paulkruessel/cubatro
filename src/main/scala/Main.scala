import controller.{GameController, IController}
import view.{IView, Tui}

@main def main(): Unit =
  val controller: IController = new GameController()
  val tui: IView = new Tui(controller)
  tui.run()
