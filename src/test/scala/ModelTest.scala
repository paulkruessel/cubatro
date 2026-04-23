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
            totalRerolls = 2,
            plays = 3,
            targetScore = 100,
            score = 0,
            phase = Phase.Draw
        )

    private def rolled(value: Int, die: Die = plainDie): RolledDie = RolledDie(die, value)

    "Model" should {
        "have all enum cases" in {
            BonusType.values.toSet.shouldBe(Set(BonusType.Chips, BonusType.Mult, BonusType.None))
            Combination.values.toSet.shouldBe(
                Set(
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
            )
            Phase.values.toSet.shouldBe(
                Set(Phase.Draw, Phase.Select, Phase.Roll, Phase.PickOut, Phase.Score, Phase.EndEval, Phase.Win, Phase.Lose)
            )
        }

        "roll die values in range and preserve config" in {
            val die = Die(2, 4, BonusType.None, 0)
            val result = die.roll()

            result.value.should(be >= 2 and be <= 4)
            result.die.min.shouldBe(2)
            result.die.max.shouldBe(4)
            result.die.bonusType.shouldBe(BonusType.None)
            result.die.bonusValue.shouldBe(0)
        }

        "evaluate rolled die with all bonus types" in {
            rolled(4, Die(1, 6, BonusType.None, 0)).eval().shouldBe((4, 0))
            rolled(4, Die(1, 6, BonusType.Chips, 3)).eval().shouldBe((7, 0))
            rolled(4, Die(1, 6, BonusType.Mult, 2)).eval().shouldBe((4, 2))
        }

        "find matching combinations correctly" in {
            val fullSet = List(rolled(2), rolled(2), rolled(2), rolled(2), rolled(2))
            matchingCombinations(fullSet).should(contain allElementsOf List(
                Combination.Twos,
                Combination.ThreeOfAKind,
                Combination.FourOfAKind,
                Combination.Yahtzee
            ))

            val fullHouse = List(rolled(2), rolled(2), rolled(3), rolled(3), rolled(3))
            matchingCombinations(fullHouse).should(contain(Combination.FullHouse))

            val largeStraight = List(rolled(2), rolled(3), rolled(4), rolled(5), rolled(6))
            val largeMatches = matchingCombinations(largeStraight)
            largeMatches.should(contain(Combination.SmallStraight))
            largeMatches.should(contain(Combination.LargeStraight))
            largeMatches.last.shouldBe(Combination.LargeStraight)
        }

        "draw up to hand size and reduce bag" in {
            val state = baseState.copy(
                bag = List(plainDie, chipsDie, multDie),
                availableDice = List(plainDie),
                maxAvailableDice = 3
            )

            val drawn = state.drawDice()
            drawn.availableDice.length.shouldBe(3)
            drawn.bag.length.shouldBe(1)
        }

        "keep full hand unchanged on draw" in {
            val state = baseState.copy(
                bag = List(plainDie, chipsDie),
                availableDice = List(plainDie, chipsDie),
                maxAvailableDice = 2
            )

            val unchanged = state.drawDice()
            unchanged.availableDice.shouldBe(state.availableDice)
            unchanged.bag.toSet.shouldBe(state.bag.toSet)
            unchanged.bag.length.shouldBe(state.bag.length)
        }

        "select valid dice, ignore invalid, and enforce max 5 selected" in {
            val state = baseState.copy(availableDice = List.fill(7)(plainDie), selectedDice = List.fill(4)(chipsDie))

            val selected = state.selectDice(List(2, 3, 4))
            selected.selectedDice.length.shouldBe(5)
            selected.availableDice.length.shouldBe(6)

            state.selectDice(List(-1, 99)).shouldBe(state)
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
            afterDiscard.discards.shouldBe(0)
            afterDiscard.selectedDice.shouldBe(Nil)
            afterDiscard.availableDice.length.shouldBe(3)
            afterDiscard.bag.length.shouldBe(0)

            baseState.copy(discards = 0, selectedDice = List(plainDie)).discardDice().shouldBe(
                baseState.copy(discards = 0, selectedDice = List(plainDie))
            )
        }

        "move selected dice to play, roll and refill hand" in {
            val state = baseState.copy(
                bag = List(chipsDie),
                availableDice = List(multDie),
                selectedDice = List(Die(3, 3, BonusType.None, 0), Die(1, 1, BonusType.None, 0)),
                maxAvailableDice = 2
            )

            val next = state.addDiceToPlay()
            next.selectedDice.shouldBe(Nil)
            next.diceInPlay.map(_.value).shouldBe(List(1, 3))
            next.availableDice.length.shouldBe(2)
        }

        "move chosen dice from play to reroll queue" in {
            val r1 = rolled(1)
            val r2 = rolled(2)
            val r3 = rolled(3)
            val state = baseState.copy(diceInPlay = List(r1, r2, r3), diceToRoll = List(rolled(6)))

            val moved = state.selectPlayedDice(List(2, 0, 2))
            moved.diceToRoll.shouldBe(List(rolled(6), r3, r1))
            moved.diceInPlay.shouldBe(List(r2))
            state.selectPlayedDice(List(-1, 99)).shouldBe(state)
        }

        "reroll queued dice only when allowed and keep sorted values" in {
            val dLow = Die(1, 1, BonusType.None, 0)
            val dHigh = Die(6, 6, BonusType.None, 0)
            val state = baseState.copy(rerolls = 2, diceInPlay = List(rolled(4)), diceToRoll = List(rolled(2, dHigh), rolled(5, dLow)))

            val rerolled = state.rollDice()
            rerolled.rerolls.shouldBe(1)
            rerolled.diceToRoll.shouldBe(Nil)
            rerolled.diceInPlay.map(_.value).shouldBe(List(1, 4, 6))

            baseState.copy(rerolls = 0, diceToRoll = List(rolled(2))).rollDice().shouldBe(
                baseState.copy(rerolls = 0, diceToRoll = List(rolled(2)))
            )
        }

        "score dice in play, lock row, clear play area and reset rerolls" in {
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
                score = 10,
                rerolls = 0,
                totalRerolls = 4
            )

            val scored = state.scoreDiceInPlay()
            scored.score.shouldBe(3122)
            scored.lockedRows.length.shouldBe(1)
            scored.lockedRows.head.combination.shouldBe(Combination.Yahtzee)
            scored.lockedRows.head.score.shouldBe(3105)
            scored.diceInPlay.shouldBe(Nil)
            scored.diceToRoll.shouldBe(Nil)
            scored.rerolls.shouldBe(4)
        }

        "not score when no dice are in play" in {
            baseState.scoreDiceInPlay().shouldBe(baseState)
        }
 

        "leave state unchanged when adding dice to play without a selection" in {
            val state = baseState.copy(
                availableDice = List(plainDie),
                selectedDice = Nil,
                diceInPlay = List(rolled(3))
            )

            state.addDiceToPlay().shouldBe(state)
        }

    }
}
