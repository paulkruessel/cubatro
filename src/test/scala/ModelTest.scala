import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers._

class ModelSpec extends AnyWordSpec {
    private val plainDie = Die(1, 6, BonusType.None, 0)
    private val chipsDie = Die(1, 6, BonusType.Chips, 3)
    private val multDie = Die(1, 6, BonusType.Mult, 2)

    private def baseState: GameState =
        GameState(
            bag = Nil,
            availableDice = Nil,
            maxAvailableDice = 5,
            selectedDice = Nil,
            diceInPlay = Nil,
            diceToRoll = Nil,
            lockedRows = Nil,
            cupgrades = Nil,
            discards = 3,
            rerolls = 2,
            plays = 3,
            targetScore = 100,
            score = 0,
            phase = Phase.Draw
        )

    private def rolled(value: Int, die: Die = plainDie): RolledDie = RolledDie(die, value)

    "Model" should {
        "have all BonusType enum cases" in {
            BonusType.values.toSet shouldBe Set(BonusType.Chips, BonusType.Mult, BonusType.None)
        }

        "have all Combination enum cases" in {
            Combination.values.toSet shouldBe Set(
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
                Combination.Yahtzee
            )
        }

        "have all Phase enum cases" in {
            Phase.values.toSet shouldBe Set(
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

        "roll die values in range and preserve die config" in {
            val die = Die(2, 4, BonusType.None, 0)
            val result = die.roll()

            result.value should (be >= 2 and be <= 4)
            result.die.min shouldBe 2
            result.die.max shouldBe 4
            result.die.bonusType shouldBe BonusType.None
            result.die.bonusValue shouldBe 0
        }

        "use default die min and max values" in {
            val dieWithDefaults = Die(bonusType = BonusType.None, bonusValue = 0)
            val result = dieWithDefaults.roll().value

            dieWithDefaults.min shouldBe 1
            dieWithDefaults.max shouldBe 6
            result should (be >= 1 and be <= 6)
        }

        "evaluate rolled die with all bonus types" in {
            rolled(4, Die(1, 6, BonusType.None, 0)).eval() shouldBe (4, 0)
            rolled(4, Die(1, 6, BonusType.Chips, 3)).eval() shouldBe (7, 0)
            rolled(4, Die(1, 6, BonusType.Mult, 2)).eval() shouldBe (4, 2)
        }

        "find matching combinations including straights and multiples" in {
            val fullSet = List(rolled(2), rolled(2), rolled(2), rolled(2), rolled(2))
            matchingCombinations(fullSet) should contain allElementsOf List(
                Combination.Twos,
                Combination.ThreeOfAKind,
                Combination.FourOfAKind,
                Combination.Yahtzee
            )

            val fullHouse = List(rolled(2), rolled(2), rolled(3), rolled(3), rolled(3))
            matchingCombinations(fullHouse) should contain(Combination.FullHouse)

            val smallStraight = List(rolled(1), rolled(2), rolled(3), rolled(4), rolled(6))
            val smallMatches = matchingCombinations(smallStraight)
            smallMatches should contain(Combination.SmallStraight)
            smallMatches should not contain Combination.LargeStraight

            val largeStraight = List(rolled(2), rolled(3), rolled(4), rolled(5), rolled(6))
            val largeMatches = matchingCombinations(largeStraight)
            largeMatches should contain(Combination.SmallStraight)
            largeMatches should contain(Combination.LargeStraight)
            largeMatches.last shouldBe Combination.LargeStraight
        }

        "draw up to hand size and reduce bag by drawn amount" in {
            val state = baseState.copy(
                bag = List(plainDie, chipsDie, multDie),
                availableDice = List(plainDie),
                maxAvailableDice = 3
            )

            val drawn = state.drawDice()
            drawn.availableDice.length shouldBe 3
            drawn.bag.length shouldBe 1
        }

        "not draw when hand is already full" in {
            val state = baseState.copy(
                bag = List(plainDie, chipsDie),
                availableDice = List(plainDie, chipsDie),
                maxAvailableDice = 2
            )

            val unchanged = state.drawDice()
            unchanged.availableDice shouldBe state.availableDice
            unchanged.bag.toSet shouldBe state.bag.toSet
            unchanged.bag.length shouldBe state.bag.length
        }

        "select dice by valid unique indices and ignore invalid input" in {
            val state = baseState.copy(availableDice = List(plainDie, chipsDie, multDie))

            val selected = state.selectDice(List(2, 2, -1, 99, 0))
            selected.selectedDice shouldBe List(multDie, plainDie)
            selected.availableDice shouldBe List(chipsDie)

            state.selectDice(List(-1, 99)) shouldBe state
        }

        "discard selected dice only when discards remain" in {
            val state = baseState.copy(
                bag = List(plainDie, chipsDie),
                availableDice = List(multDie),
                selectedDice = List(plainDie),
                discards = 1,
                maxAvailableDice = 3
            )

            val afterDiscard = state.discardDice()
            afterDiscard.discards shouldBe 0
            afterDiscard.selectedDice shouldBe Nil
            afterDiscard.availableDice.length shouldBe 3
            afterDiscard.bag.length shouldBe 0

            baseState.copy(discards = 0, selectedDice = List(plainDie)).discardDice() shouldBe
                baseState.copy(discards = 0, selectedDice = List(plainDie))
            baseState.copy(discards = 2, selectedDice = Nil).discardDice() shouldBe
                baseState.copy(discards = 2, selectedDice = Nil)
        }

        "move selected dice to play, roll them and refill hand" in {
            val state = baseState.copy(
                bag = List(chipsDie),
                availableDice = List(multDie),
                selectedDice = List(plainDie),
                maxAvailableDice = 2
            )

            val next = state.addDiceToPlay()
            next.selectedDice shouldBe Nil
            next.diceInPlay.length shouldBe 1
            next.availableDice.length shouldBe 2

            baseState.addDiceToPlay() shouldBe baseState
        }

        "move chosen dice from play to reroll queue" in {
            val r1 = rolled(1)
            val r2 = rolled(2)
            val r3 = rolled(3)
            val state = baseState.copy(diceInPlay = List(r1, r2, r3), diceToRoll = List(rolled(6)))

            val moved = state.selectPlayedDice(List(2, 0, 2))
            moved.diceToRoll shouldBe List(rolled(6), r3, r1)
            moved.diceInPlay shouldBe List(r2)

            state.selectPlayedDice(List(-1, 99)) shouldBe state
        }

        "reroll queued dice only when allowed and consume one reroll" in {
            val state = baseState.copy(rerolls = 2, diceToRoll = List(rolled(2), rolled(5)))

            val rerolled = state.rollDice()
            rerolled.rerolls shouldBe 1
            rerolled.diceToRoll shouldBe Nil
            rerolled.diceInPlay.length shouldBe 2

            baseState.copy(rerolls = 0, diceToRoll = List(rolled(2))).rollDice() shouldBe
                baseState.copy(rerolls = 0, diceToRoll = List(rolled(2)))
            baseState.copy(rerolls = 2, diceToRoll = Nil).rollDice() shouldBe
                baseState.copy(rerolls = 2, diceToRoll = Nil)
        }

        "score dice in play, lock row, clear play area and apply cupgrades" in {
            val dice = List(
                rolled(1, Die(1, 6, BonusType.Chips, 2)),
                rolled(1, Die(1, 6, BonusType.Mult, 3)),
                rolled(1, Die(1, 6, BonusType.None, 0)),
                rolled(1, Die(1, 6, BonusType.None, 0)),
                rolled(1, Die(1, 6, BonusType.None, 0))
            )
            val bonusCupgrade = Cupgrade("bonus", state => state.copy(score = state.score + 7))

            val state = baseState.copy(
                diceInPlay = dice,
                diceToRoll = List(rolled(6)),
                cupgrades = List(bonusCupgrade),
                score = 10
            )

            val scored = state.scoreDiceInPlay()
            // Dice chips: (1+2)+1+1+1+1 = 7; mult: 3; highest combo is Yahtzee (chips 200, mult 12)
            // => (7 + 200) * (3 + 12) = 3105. Score starts at 10, then +3105 and cupgrade +7 => 3122.
            scored.score shouldBe 3122
            scored.lockedRows.length shouldBe 1
            scored.lockedRows.head.combination shouldBe Combination.Yahtzee
            scored.lockedRows.head.score shouldBe 3105
            scored.diceInPlay shouldBe Nil
            scored.diceToRoll shouldBe Nil
        }

        "not score when no dice are in play" in {
            baseState.scoreDiceInPlay() shouldBe baseState
        }
    }
}