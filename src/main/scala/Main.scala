import di.{AppInjector, DefaultAppModule}

import java.awt.GraphicsEnvironment

@main def main(): Unit =
  runApp()

def runApp(
    isHeadless: Boolean = GraphicsEnvironment.isHeadless(),
    createInjector: () => AppInjector = () => AppInjector.from(new DefaultAppModule())
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
