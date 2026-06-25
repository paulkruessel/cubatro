package fileio

import model.*
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.Files

class FileIOTest extends AnyWordSpec with Matchers:

  private val plain = DieFactory.plain
  private val chip = DieFactory.chips(2)
  private val mult = DieFactory.mult(2)

  private def state: GameState =
    GameState(
      bag = List(plain, chip, mult),
      availableDice = List(chip, plain),
      maxAvailableDice = 8,
      selectedDice = List(mult),
      diceInPlay = List(RolledDie(plain, 5), RolledDie(chip, 6)),
      diceToRoll = List(RolledDie(mult, 3)),
      lockedRows = List(
        LockedRow(
          dice = List(RolledDie(plain, 6), RolledDie(chip, 6)),
          combination = Combination.Sixes,
          score = 43
        )
      ),
      cupgrades = List(Cupgrade("saved-upgrade", identity)),
      discards = 3,
      rerolls = 2,
      totalRerolls = 4,
      plays = 5,
      targetScore = 1000,
      score = 123,
      phase = Phase.PickOut
    )

  private def assertLoadedState(loaded: GameState): Unit =
    loaded.bag shouldBe state.bag
    loaded.availableDice shouldBe state.availableDice
    loaded.maxAvailableDice shouldBe state.maxAvailableDice
    loaded.selectedDice shouldBe state.selectedDice
    loaded.diceInPlay shouldBe state.diceInPlay
    loaded.diceToRoll shouldBe state.diceToRoll
    loaded.lockedRows shouldBe state.lockedRows
    loaded.cupgrades.map(_.name) shouldBe List("saved-upgrade")
    loaded.discards shouldBe state.discards
    loaded.rerolls shouldBe state.rerolls
    loaded.totalRerolls shouldBe state.totalRerolls
    loaded.plays shouldBe state.plays
    loaded.targetScore shouldBe state.targetScore
    loaded.score shouldBe state.score
    loaded.phase shouldBe state.phase

  "XmlFileIO" should {
    "save and load a GameState" in {
      val file = Files.createTempFile("cubatro", ".xml")
      val fileIO: FileIO = new XmlFileIO()

      fileIO.save(state, file.toString)
      val loaded = fileIO.load(file.toString)

      assertLoadedState(loaded)
      Files.deleteIfExists(file)
    }
  }


    "load cupgrade effect as identity function" in {
      val file = Files.createTempFile("cubatro", ".xml")
      val fileIO: FileIO = new XmlFileIO()

      fileIO.save(state, file.toString)
      val loaded = fileIO.load(file.toString)
      val unchanged = loaded.cupgrades.head.effect(loaded)

      unchanged shouldBe loaded

      Files.deleteIfExists(file)
    }

  "JsonFileIO" should {
    "save and load a GameState" in {
      val file = Files.createTempFile("cubatro", ".json")
      val fileIO: FileIO = new JsonFileIO()

      fileIO.save(state, file.toString)
      val loaded = fileIO.load(file.toString)

      assertLoadedState(loaded)
      Files.deleteIfExists(file)
    }

    "load cupgrade effect as identity function" in {
      val file = Files.createTempFile("cubatro", ".json")
      val fileIO: FileIO = new JsonFileIO()

      fileIO.save(state, file.toString)
      val loaded = fileIO.load(file.toString)
      val unchanged = loaded.cupgrades.head.effect(loaded)

      unchanged shouldBe loaded

      Files.deleteIfExists(file)
    }
  }
