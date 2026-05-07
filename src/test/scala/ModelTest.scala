import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import model.*

class ModelSpec extends AnyWordSpec with Matchers {

  private val plainDie = Die(bonusType = BonusType.None, bonusValue = 0)
  private val chipDie = Die(bonusType = BonusType.Chips, bonusValue = 2)
  private val multDie = Die(bonusType = BonusType.Mult, bonusValue = 1)

  private def baseState: GameState =
    GameState(
      bag = List.fill(10)(plainDie),
      availableDice = List(plainDie, chipDie, multDie),
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
      score = 0,
      phase = Phase.Select
    )

  "Model" should {

    "have all BonusType enum cases" in {
      BonusType.values.toSet shouldBe Set(BonusType.Chips, BonusType.Mult, BonusType.None)
    }

    "have main Phase enum cases" in {
      Phase.values.toSet should contain allOf (
        Phase.Draw,
        Phase.Select,
        Phase.Roll,
        Phase.PickOut,
        Phase.Score,
        Phase.EndEval,
        Phase.Win,
        Phase.Lose
      )
    }

    "roll die values in range" in {
      val rolled = plainDie.roll()
      rolled.value should be >= 1
      rolled.value should be <= 6
    }

    "evaluate all die bonus types" in {
      RolledDie(plainDie, 3).eval() shouldBe (3, 0)
      RolledDie(chipDie, 3).eval() shouldBe (5, 0)
      RolledDie(multDie, 3).eval() shouldBe (3, 1)
    }

    "find matching combinations" in {
      matchingCombinations(List(
        RolledDie(plainDie, 1),
        RolledDie(plainDie, 2),
        RolledDie(plainDie, 3),
        RolledDie(plainDie, 4),
        RolledDie(plainDie, 5)
      )) should contain (Combination.LargeStraight)

      matchingCombinations(List.fill(5)(RolledDie(plainDie, 6))) should contain (Combination.Yahtzee)
    }
  }
}
