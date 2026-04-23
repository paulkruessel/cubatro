import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers._

class ModelSpec extends AnyWordSpec {
    private def baseState: GameState =
        GameState(
            bag = Nil,
            availableDice = Nil,
            selectedDice = Nil,
            diceToRoll = Nil,
            draftRow = Nil,
            lockedRows = Nil,
            cupgrades = Nil,
            discards = 3,
            targetScore = 100,
            score = 0,
            phase = Phase.Draw
        )

    "Model" should {
        "have all BonusType enum cases" in {
            val all = BonusType.values.toSet
            all.shouldBe(Set(BonusType.Chips, BonusType.Mult, BonusType.None))
        }

        "have all Combination enum cases" in {
            val all = Combination.values.toSet
            all.shouldBe(Set(
                Combination.Ones,
                Combination.Twos,
                Combination.Threes,
                Combination.Fours,
                Combination.Fives,
                Combination.Sixes,
                Combination.ThreeOfAKind,
                Combination.FourOfAKind,
                Combination.FullHouse,
                Combination.SmallStraight,
                Combination.LargeStraight,
                Combination.Yahtzee,
                Combination.Chance
            ))
        }

        "have all Phase enum cases" in {
            val all = Phase.values.toSet
            all.shouldBe(
                Set(Phase.Draw, Phase.Select, Phase.Roll, Phase.PickOut, Phase.Score, Phase.EndEval, Phase.Win, Phase.Lose)
            )
        }

        "roll die values in range" in {
            val die = Die(1, 6, BonusType.None, 0)
            val result = die.roll()
            result.should(be >= 1 and be <= 6)
        }

        "use default die min and max values" in {
            val dieWithDefaults = Die(bonusType = BonusType.None, bonusValue = 0)
            val result = dieWithDefaults.roll()

            dieWithDefaults.min.shouldBe(1)
            dieWithDefaults.max.shouldBe(6)
            result.should(be >= 1 and be <= 6)
        }

        "evaluate die with None bonus without changing state" in {
            val die = Die(1, 6, BonusType.None, 0)
            val state = baseState
            val (value, updatedState) = die.eval(state)
            value.should(be >= 1 and be <= 6)
            updatedState.shouldBe(state)
        }

        "evaluate die with Chips bonus by adding chips value" in {
            val die = Die(1, 6, BonusType.Chips, 3)
            val state = baseState
            val (value, updatedState) = die.eval(state)
            value.should(be >= 4 and be <= 9)
            updatedState.shouldBe(state)
        }

        "evaluate die with Mult bonus by multiplying roll" in {
            val die = Die(1, 6, BonusType.Mult, 2)
            val state = baseState
            val (value, updatedState) = die.eval(state)
            value.should(be >= 2 and be <= 12)
            updatedState.shouldBe(state)
        }

        "calculate total score as chips times mult" in {
            val score = Score(chips = 7, mult = 4)
            score.total.shouldBe(28)
        }

        "construct RolledDie and LockedRow" in {
            val die = Die(1, 6, BonusType.None, 0)
            val rolled = RolledDie(die, 3)
            val score = Score(3, 2)
            val row = LockedRow(List(rolled), Combination.Ones, score)

            rolled.die.shouldBe(die)
            rolled.value.shouldBe(3)
            row.dice.shouldBe(List(rolled))
            row.combination.shouldBe(Combination.Ones)
            row.score.total.shouldBe(6)
        }

        "roll(die) should return a copy with updated rolledValue" in {
            val die = Die(1, 6, BonusType.None, 0, rolledValue = 0)
            val rolled = die.roll(die)

            rolled.min.shouldBe(die.min)
            rolled.max.shouldBe(die.max)
            rolled.bonusType.shouldBe(die.bonusType)
            rolled.bonusValue.shouldBe(die.bonusValue)
            rolled.rolledValue.should(be >= 1 and be <= 6)
        }
    }
}