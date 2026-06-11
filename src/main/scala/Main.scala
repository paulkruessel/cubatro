import controller.{GameController, IController}
import view.{Gui, IView, Tui}

import java.awt.GraphicsEnvironment
import scala.swing.Swing

@main def main(): Unit =
  runApp()

def runApp(
    isHeadless: Boolean = GraphicsEnvironment.isHeadless(),
    createController: () => IController = () => new GameController(),
    createTui: IController => IView = controller => new Tui(controller),
    startGui: IController => Unit = controller =>
      Swing.onEDT {
        val gui = new Gui(controller)
        gui.visible = true
      }
): Unit =
  val controller = createController()
  val tui = createTui(controller)

  controller.start()

  if !isHeadless then
    startGui(controller)
  else
    println("GUI not started: no graphical display available.")

  tui.run()