package di

import fileio.*
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class AppModuleFileIOTest extends AnyWordSpec with Matchers:

  "DefaultAppModule" should {
    "use JsonFileIO by default" in {
      val module = new DefaultAppModule(guiLauncher = _ => ())
      module.fileIO shouldBe a [JsonFileIO]
    }

    "allow switching FileIO implementation by dependency injection" in {
      val module = new DefaultAppModule(
        guiLauncher = _ => (),
        fileIOProvider = () => new XmlFileIO()
      )

      val injector = AppInjector.from(module)

      injector.fileIO shouldBe a [XmlFileIO]
    }
  }
