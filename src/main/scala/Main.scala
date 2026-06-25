import di.AppInjector

import java.awt.GraphicsEnvironment

@main def main(): Unit =
  runApp()

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
