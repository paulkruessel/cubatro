import controller.GameController
import view.{Gui, Tui}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.swing.Swing

@main def main(): Unit =
  val controller = new GameController()
  val tui = new Tui(controller)

  Swing.onEDT {
    val gui = new Gui(controller)
    gui.visible = true
  }

  Future {
    tui.run()
  }