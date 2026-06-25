import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import controller.{GameController, IController}
import di.{AppInjector, AppModule}
import fileio.{FileIO, JsonFileIO}
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import view.{IView, Tui}

class MainTest extends AnyWordSpec with Matchers:

  private def testInjector(startGuiCallback: () => Unit = () => ()): AppInjector =
    AppInjector.from(new AppModule:
      override val fileIO: FileIO = new JsonFileIO()

      override val controller: IController =
        new GameController(fileIO = fileIO)

      override def tui(using controller: IController): IView =
        new Tui(controller)

      override def startGui(using controller: IController): Unit =
        startGuiCallback()
    )

  "Main" should {

    "run the headless branch through the testable helper" in {
      val input = new ByteArrayInputStream("q\n".getBytes("UTF-8"))
      val output = new ByteArrayOutputStream()
      var guiStarted = false

      Console.withIn(input) {
        Console.withOut(output) {
          runApp(
            isHeadless = true,
            createInjector = () => testInjector(() => guiStarted = true)
          )
        }
      }

      guiStarted shouldBe false
      val text = output.toString("UTF-8")
      text should include("GUI not started: no graphical display available.")
      text should include("Game stopped by player.")
    }

    "run the gui branch through the testable helper" in {
      val input = new ByteArrayInputStream("q\n".getBytes("UTF-8"))
      val output = new ByteArrayOutputStream()
      var guiStarted = false

      Console.withIn(input) {
        Console.withOut(output) {
          runApp(
            isHeadless = false,
            createInjector = () => testInjector(() => guiStarted = true)
          )
        }
      }

      guiStarted shouldBe true
      val text = output.toString("UTF-8")
      text should not include("GUI not started: no graphical display available.")
      text should include("Game stopped by player.")
    }
  }
