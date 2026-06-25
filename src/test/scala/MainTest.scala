import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import com.google.inject.AbstractModule
import controller.{GameController, IController}
import di.{AppInjector, GuiLauncher}
import fileio.{FileIO, JsonFileIO}
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import view.{IView, Tui}

class MainTest extends AnyWordSpec with Matchers:

  private def testInjector(startGuiCallback: () => Unit = () => ()): AppInjector =
    val fileIO: FileIO = new JsonFileIO()
    val controller: IController = new GameController(fileIO = fileIO)
    val tui: IView = new Tui(controller)

    AppInjector.from(new AbstractModule:
      override def configure(): Unit =
        bind(classOf[FileIO]).toInstance(fileIO)
        bind(classOf[IController]).toInstance(controller)
        bind(classOf[IView]).toInstance(tui)
        bind(classOf[GuiLauncher]).toInstance(new GuiLauncher:
          override def start(controller: IController): Unit =
            startGuiCallback()
        )
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
