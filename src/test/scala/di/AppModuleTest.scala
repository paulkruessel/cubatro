import com.google.inject.AbstractModule
import controller.*
import di.*
import fileio.{FileIO, JsonFileIO}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import view.{IView, Tui}

class AppModuleTest extends AnyWordSpec with Matchers:

  private class FakeView extends IView:
    override def update(): Unit = ()
    override def run(): Unit = ()
    override def parse(input: String): GameCommand = GameCommand.Invalid
    override def render(viewState: GameViewState): String = ""
    override def prompt(phase: String): String = ""
    override def help(phase: String): String = ""

  private class RecordingGuiLauncher extends GuiLauncher:
    var controller: Option[IController] = None

    override def start(controller: IController): Unit =
      this.controller = Some(controller)

  private class RecordingModule(guiLauncher: RecordingGuiLauncher) extends AbstractModule:
    val fileIO: FileIO = new JsonFileIO()
    val controller: IController = new GameController(fileIO = fileIO)
    val view: IView = new FakeView

    override def configure(): Unit =
      bind(classOf[FileIO]).toInstance(fileIO)
      bind(classOf[IController]).toInstance(controller)
      bind(classOf[IView]).toInstance(view)
      bind(classOf[GuiLauncher]).toInstance(guiLauncher)

  "AppInjector" should {

    "create the default injector through the factory" in {
      val injector = AppInjector.create()

      injector.controller shouldBe a [GameController]
      injector.tui shouldBe a [Tui]
      injector.fileIO shouldBe a [JsonFileIO]
    }

    "create components through the configured Guice module" in {
      val guiLauncher = new RecordingGuiLauncher
      val module = new RecordingModule(guiLauncher)
      val injector = AppInjector.from(module)

      injector.fileIO shouldBe module.fileIO
      injector.controller shouldBe module.controller
      injector.tui shouldBe module.view

      injector.startGui()

      guiLauncher.controller shouldBe Some(module.controller)
    }

    "wire the default bindings through Guice" in {
      val module = new DefaultAppModule
      val injector = AppInjector.from(module)

      injector.controller shouldBe a [GameController]
      injector.tui shouldBe a [Tui]
      injector.fileIO shouldBe a [JsonFileIO]
    }

    "wire the configured GUI launcher without opening Swing" in {
      val guiLauncher = new RecordingGuiLauncher
      val injector = AppInjector.from(new DefaultAppModule(guiLauncher = guiLauncher))

      injector.startGui()

      guiLauncher.controller shouldBe Some(injector.controller)
    }
  }

  "SwingGuiLauncher" should {

    "delegate starting the GUI to its configured launcher" in {
      val controller = new GameController()
      var startedWith: Option[IController] = None

      new SwingGuiLauncher(startedController => startedWith = Some(startedController)).start(controller)

      startedWith shouldBe Some(controller)
    }
  }
