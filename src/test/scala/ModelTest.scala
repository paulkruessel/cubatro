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

    "select dice from available dice" in {
      val state = baseState.selectDice(List(0, 1))
      state.availableDice.length shouldBe 1
      state.selectedDice.length shouldBe 2
    }

    "ignore invalid selection indices" in {
      baseState.selectDice(List(999)) shouldBe baseState
    }

    "move selected dice to play" in {
      val selected = baseState.selectDice(List(0, 1))
      val played = selected.addDiceToPlay()

      played.selectedDice shouldBe Nil
      played.diceInPlay.length shouldBe 2
    }

    "not move dice to play if no dice are selected" in {
      baseState.addDiceToPlay() shouldBe baseState
    }

    "discard selected dice and reduce discards" in {
      val selected = baseState.selectDice(List(0))
      val discarded = selected.discardDice()

      discarded.discards shouldBe 3
      discarded.selectedDice shouldBe Nil
    }

    "not discard if nothing selected or no discards left" in {
      baseState.discardDice() shouldBe baseState
      baseState.copy(selectedDice = List(plainDie), discards = 0).discardDice() shouldBe
        baseState.copy(selectedDice = List(plainDie), discards = 0)
    }

    "select played dice for reroll" in {
      val state = baseState.copy(
        diceInPlay = List(
          RolledDie(plainDie, 1),
          RolledDie(chipDie, 2)
        )
      )

      val selected = state.selectPlayedDice(List(0))

      selected.diceInPlay.length shouldBe 1
      selected.diceToRoll.length shouldBe 1
    }

    "ignore invalid played dice indices" in {
      baseState.selectPlayedDice(List(999)) shouldBe baseState
    }

    "reroll selected dice" in {
      val state = baseState.copy(
        diceToRoll = List(RolledDie(plainDie, 1)),
        rerolls = 2
      )

      val rolled = state.rollDice()

      rolled.diceToRoll shouldBe Nil
      rolled.diceInPlay.length shouldBe 1
      rolled.rerolls shouldBe 1
    }

    "not reroll without rerolls or dice to roll" in {
      val noRerolls = baseState.copy(
        rerolls = 0,
        diceToRoll = List(RolledDie(plainDie, 1))
      )

      val noDiceToRoll = baseState.copy(
        rerolls = 2,
        diceToRoll = Nil
      )

      noRerolls.rollDice() shouldBe noRerolls
      noDiceToRoll.rollDice() shouldBe noDiceToRoll
    }

    "score dice in play and create locked row" in {
      val state = baseState.copy(
        diceInPlay = List(
          RolledDie(plainDie, 6),
          RolledDie(plainDie, 6),
          RolledDie(chipDie, 1)
        )
      )

      val scored = state.scoreDiceInPlay()

      scored.score should be > 0
      scored.lockedRows.length shouldBe 1
      scored.diceInPlay shouldBe Nil
    }

    "score dice with cupgrade effect" in {
      val state = baseState.copy(
        diceInPlay = List(
          RolledDie(plainDie, 6),
          RolledDie(plainDie, 6)
        ),
        cupgrades = List(Cupgrade("bonus", s => s.copy(score = s.score + 10)))
      )

      val scored = state.scoreDiceInPlay()

      scored.score should be > 10
    }

    "not score empty dice list" in {
      baseState.scoreDiceInPlay() shouldBe baseState
    }
  }
}
