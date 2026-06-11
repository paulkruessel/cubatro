import controller.GameController
import view.{Gui, Tui}

import java.awt.GraphicsEnvironment
import scala.swing.Swing

@main def main(): Unit =
  runApp()

def runApp(
    isHeadless: Boolean = GraphicsEnvironment.isHeadless(),
    createTui: GameController => Tui = controller => new Tui(controller),
    startGui: GameController => Unit = controller =>
      Swing.onEDT {
        val gui = new Gui(controller)
        gui.visible = true
      }
): Unit =
  val controller = new GameController()
  val tui = createTui(controller)

  controller.start()

  if !isHeadless then
    startGui(controller)
  else
    println("GUI not started: no graphical display available.")

  tui.run()