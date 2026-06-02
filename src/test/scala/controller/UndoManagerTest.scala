import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import controller.*
import model.*

class UndoManagerTest extends AnyWordSpec with Matchers {

  private val plain = Die(bonusType = BonusType.None, bonusValue = 0)

  private def gameState(score: Int): GameState =
    GameState(
      bag = Nil,
      availableDice = Nil,
      maxAvailableDice = 8,
      selectedDice = Nil,
      diceInPlay = Nil,
      diceToRoll = Nil,
      lockedRows = Nil,
      cupgrades = Nil,
      discards = 4,
      rerolls = 4,
      totalRerolls = 4,
      plays = 6,
      targetScore = 1000,
      score = score,
      phase = Phase.Select
    )

  "UndoManager" should {

    "return None when undo or redo stacks are empty" in {
      val manager = new UndoManager()

      manager.undoStep() shouldBe None
      manager.redoStep() shouldBe None
    }

    "execute, undo and redo a command" in {
      val before = gameState(score = 0)
      val after = gameState(score = 10)
      var current = before
      val command = GameStateCommand(before, after, state => current = state)
      val manager = new UndoManager()

      manager.doStep(command)
      current shouldBe after

      manager.undoStep() shouldBe Some(command)
      current shouldBe before

      manager.redoStep() shouldBe Some(command)
      current shouldBe after
    }

    "clear redo stack when a new command is executed after undo" in {
      val state0 = gameState(score = 0)
      val state1 = gameState(score = 10)
      val state2 = gameState(score = 20)
      var current = state0

      val first = GameStateCommand(state0, state1, state => current = state)
      val second = GameStateCommand(state0, state2, state => current = state)
      val manager = new UndoManager()

      manager.doStep(first)
      manager.undoStep() shouldBe Some(first)
      current shouldBe state0

      manager.doStep(second)
      current shouldBe state2
      manager.redoStep() shouldBe None
    }
  }
}
