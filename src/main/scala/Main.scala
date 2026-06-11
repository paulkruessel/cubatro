import controller.GameController
import view.{Gui, Tui}

import java.awt.GraphicsEnvironment
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.swing.Swing

@main def main(): Unit =
  val controller = new GameController()
  val tui = new Tui(controller)

  controller.start()

  if !GraphicsEnvironment.isHeadless() then
    Swing.onEDT {
      val gui = new Gui(controller)
      gui.visible = true
    }
  else
    println("GUI not started: no graphical display available.")

  Future {
    tui.run()
  }