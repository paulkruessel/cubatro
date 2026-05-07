import controller.GameController
import view.Tui

@main def main(): Unit =
  val controller = new GameController()
  val tui = new Tui(controller)
  tui.run()
