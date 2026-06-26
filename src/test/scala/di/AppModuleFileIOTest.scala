package di

import controller.IController
import fileio.*
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class AppModuleFileIOTest extends AnyWordSpec with Matchers:

  private object NoOpGuiLauncher extends GuiLauncher:
    override def start(controller: IController): Unit = ()

  "DefaultAppModule" should {
    "use JsonFileIO by default" in {
      val module = new DefaultAppModule(guiLauncher = NoOpGuiLauncher)
      val injector = AppInjector.from(module)

      injector.fileIO shouldBe a [JsonFileIO]
    }

    "allow switching FileIO implementation by dependency injection" in {
      val module = new DefaultAppModule(
        fileIOImplementation = classOf[XmlFileIO],
        guiLauncher = NoOpGuiLauncher
      )

      val injector = AppInjector.from(module)

      injector.fileIO shouldBe a [XmlFileIO]
    }

    "allow switching FileIO implementation through the injector factory" in {
      val injector = AppInjector.create(fileIOImplementation = classOf[XmlFileIO])

      injector.fileIO shouldBe a [XmlFileIO]
      injector.controller.defaultSavePath shouldBe "cubatro-save.xml"
    }
  }
