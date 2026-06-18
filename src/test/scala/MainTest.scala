import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import controller.{GameController, IController}
import di.{AppInjector, AppModule}
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.awt.Frame
import javax.swing.SwingUtilities
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import view.{IView, Tui}

class MainTest extends AnyWordSpec with Matchers:

  private def testInjector(startGuiCallback: () => Unit = () => ()): AppInjector =
    AppInjector.from(new AppModule:
      override val controller: IController = new GameController()

      override def tui(using controller: IController): IView =
        new Tui(controller)

      override def startGui(using controller: IController): Unit =
        startGuiCallback()
    )

  private def disposeAllFrames(): Unit =
    try
      SwingUtilities.invokeAndWait(() => {
        Frame.getFrames.foreach(_.dispose())
      })
    catch
      case _: Throwable => ()

  "Main" should {

    "start and quit" in {
      val input = new ByteArrayInputStream("q\n".getBytes("UTF-8"))
      val output = new ByteArrayOutputStream()

      val runMain = Future {
        try
          Console.withIn(input) {
            Console.withOut(output) {
              main()
            }
          }
        finally
          disposeAllFrames()
      }

      Await.result(runMain, 5.seconds)

      val text = output.toString("UTF-8")
      text should include("CUBATRO")
      text should include("Select phase")
      text should include("Game stopped by player.")
    }

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
    }
  }
