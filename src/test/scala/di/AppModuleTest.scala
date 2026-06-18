import controller.*
import di.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import view.{IView, Tui}

import java.awt.{Frame, GraphicsEnvironment}
import javax.swing.SwingUtilities

class AppModuleTest extends AnyWordSpec with Matchers:

  private class FakeView extends IView:
    override def update(): Unit = ()
    override def run(): Unit = ()
    override def parse(input: String): GameCommand = GameCommand.Invalid
    override def render(viewState: GameViewState): String = ""
    override def prompt(phase: String): String = ""
    override def help(phase: String): String = ""

  private class RecordingModule extends AppModule:
    override val controller: IController = new GameController()
    var tuiController: Option[IController] = None
    var guiController: Option[IController] = None

    override def tui(using controller: IController): IView =
      tuiController = Some(controller)
      new FakeView

    override def startGui(using controller: IController): Unit =
      guiController = Some(controller)

  private def flushEdt(): Unit =
    SwingUtilities.invokeAndWait(() => ())

  private def cubatroFrames: Seq[Frame] =
    Frame.getFrames.toSeq.filter(_.getTitle == "Cubatro")

  private def disposeCubatroFrames(): Unit =
    SwingUtilities.invokeAndWait(() => cubatroFrames.foreach(_.dispose()))

  "AppInjector" should {

    "create components through the configured module" in {
      val module = new RecordingModule
      val injector = AppInjector.from(module)

      injector.controller shouldBe module.controller
      injector.tui shouldBe a [FakeView]
      module.tuiController shouldBe Some(module.controller)

      injector.startGui()

      module.guiController shouldBe Some(module.controller)
    }

    "wire the default controller into the default TUI" in {
      val module = new DefaultAppModule
      val injector = AppInjector.from(module)

      injector.controller shouldBe module.controller
      injector.tui shouldBe a [Tui]
    }

    "start the default GUI as a visible frame" in {
      assume(!GraphicsEnvironment.isHeadless, "Skipping Swing GUI test in headless environment.")

      val module = new DefaultAppModule

      try
        module.startGui(using module.controller)
        flushEdt()

        cubatroFrames.exists(_.isVisible) shouldBe true
      finally
        disposeCubatroFrames()
    }
  }
