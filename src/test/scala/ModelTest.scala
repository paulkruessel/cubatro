import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ModelTest extends AnyFunSuite with Matchers {
    private def baseState: GameState =
        GameState(
            bag = Nil,
            availableDice = Nil,
            rolledDice = Nil,
            draftRow = Nil,
            lockedRows = Nil,
            cupgrades = Nil,
            rerollsLeft = 3,
            targetScore = 100,
            money = 10,
            usedCombinations = Set.empty,
            phase = Phase.Draw
        )

    test("BonusType enum should expose all cases") {
        val all = BonusType.values.toSet
        all shouldBe Set(BonusType.Chips, BonusType.Mult, BonusType.Money, BonusType.None)
    }

    test("Combination enum should expose all cases") {
        val all = Combination.values.toSet
        all shouldBe Set(
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
        )
    }

    test("ConsoleColors enum and apply should work for all cases") {
        val text = "x"
        val all = ConsoleColors.values
        all.length shouldBe 8
        all.foreach { color =>
            val colored = color(text)
            colored.startsWith(color.code) shouldBe true
            colored.endsWith(ConsoleColors.CLEAR.code) shouldBe true
            colored.contains(text) shouldBe true
        }
    }

    test("Phase enum should expose all cases") {
        val all = Phase.values.toSet
        all shouldBe Set(Phase.Draw, Phase.Select, Phase.Roll, Phase.Lock, Phase.Score, Phase.Shop)
    }

    test("Die roll should stay in range") {
        val die = Die(1, 6, BonusType.None, 0)
        val result = die.roll()
        result should (be >= 1 and be <= 6)
    }

    test("Die should use default min and max values") {
        val dieWithDefaults = Die(bonusType = BonusType.None, bonusValue = 0)
        val result = dieWithDefaults.roll()

        dieWithDefaults.min shouldBe 1
        dieWithDefaults.max shouldBe 6
        result should (be >= 1 and be <= 6)
    }

    test("Die eval with None bonus should keep state and value in range") {
        val die = Die(1, 6, BonusType.None, 0)
        val state = baseState
        val (value, updatedState) = die.eval(state)
        value should (be >= 1 and be <= 6)
        updatedState shouldBe state
    }

    test("Die eval with Chips bonus should add chips bonus") {
        val die = Die(1, 6, BonusType.Chips, 3)
        val state = baseState
        val (value, updatedState) = die.eval(state)
        value should (be >= 4 and be <= 9)
        updatedState shouldBe state
    }

    test("Die eval with Mult bonus should apply multiplier") {
        val die = Die(1, 6, BonusType.Mult, 2)
        val state = baseState
        val (value, updatedState) = die.eval(state)
        value should (be >= 2 and be <= 12)
        updatedState shouldBe state
    }

    test("Die eval with Money bonus should add money") {
        val die = Die(1, 6, BonusType.Money, 5)
        val state = baseState
        val (value, updatedState) = die.eval(state)
        value should (be >= 1 and be <= 6)
        updatedState.money shouldBe state.money + 5
        updatedState.phase shouldBe state.phase
    }

    test("Score total should multiply chips and mult") {
        val score = Score(chips = 7, mult = 4)
        score.total shouldBe 28
    }

    test("GameState addMoney should return updated immutable copy") {
        val state = baseState.copy(
            bag = List(Die(1, 6, BonusType.None, 0)),
            phase = Phase.Roll,
            usedCombinations = Set(Combination.Chance)
        )

        val updated = state.addMoney(12)

        updated.money shouldBe state.money + 12
        updated.bag shouldBe state.bag
        updated.phase shouldBe state.phase
        updated.usedCombinations shouldBe state.usedCombinations
    }

    test("RolledDie, Cupgrade and LockedRow should be constructible") {
        val die = Die(1, 6, BonusType.None, 0)
        val rolled = RolledDie(die, 3)
        val cupgrade = Cupgrade("+1 money", _.addMoney(1))
        val score = Score(3, 2)
        val row = LockedRow(List(rolled), Combination.Ones, score)
        val state = baseState

        rolled.die shouldBe die
        rolled.value shouldBe 3
        cupgrade.name shouldBe "+1 money"
        cupgrade.effect(state).money shouldBe state.money + 1
        row.dice shouldBe List(rolled)
        row.combination shouldBe Combination.Ones
        row.score.total shouldBe 6
    }
}