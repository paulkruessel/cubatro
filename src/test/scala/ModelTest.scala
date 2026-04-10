class ModelTest extends munit.FunSuite {
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
        assertEquals(all, Set(BonusType.Chips, BonusType.Mult, BonusType.Money, BonusType.None))
    }

    test("Combination enum should expose all cases") {
        val all = Combination.values.toSet
        assertEquals(
            all,
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
                Combination.Yahtzee,
                Combination.Chance
            )
        )
    }

    test("ConsoleColors enum and apply should work for all cases") {
        val text = "x"
        val all = ConsoleColors.values
        assertEquals(all.length, 8)
        all.foreach { color =>
            val colored = color(text)
            assert(colored.startsWith(color.code))
            assert(colored.endsWith(ConsoleColors.CLEAR.code))
            assert(colored.contains(text))
        }
    }

    test("Phase enum should expose all cases") {
        val all = Phase.values.toSet
        assertEquals(all, Set(Phase.Draw, Phase.Select, Phase.Roll, Phase.Lock, Phase.Score, Phase.Shop))
    }

    test("Die roll should stay in range") {
        val die = Die(1, 6, BonusType.None, 0)
        val result = die.roll()
        assert(result >= 1 && result <= 6)
    }

    test("Die eval with None bonus should keep state and value in range") {
        val die = Die(1, 6, BonusType.None, 0)
        val state = baseState
        val (value, updatedState) = die.eval(state)
        assert(value >= 1 && value <= 6)
        assertEquals(updatedState, state)
    }

    test("Die eval with Chips bonus should add chips bonus") {
        val die = Die(1, 6, BonusType.Chips, 3)
        val state = baseState
        val (value, updatedState) = die.eval(state)
        assert(value >= 4 && value <= 9)
        assertEquals(updatedState, state)
    }

    test("Die eval with Mult bonus should apply multiplier") {
        val die = Die(1, 6, BonusType.Mult, 2)
        val state = baseState
        val (value, updatedState) = die.eval(state)
        assert(value >= 2 && value <= 12)
        assertEquals(updatedState, state)
    }

    test("Die eval with Money bonus should add money") {
        val die = Die(1, 6, BonusType.Money, 5)
        val state = baseState
        val (value, updatedState) = die.eval(state)
        assert(value >= 1 && value <= 6)
        assertEquals(updatedState.money, state.money + 5)
        assertEquals(updatedState.phase, state.phase)
    }

    test("Score total should multiply chips and mult") {
        val score = Score(chips = 7, mult = 4)
        assertEquals(score.total, 28)
    }

    test("GameState addMoney should return updated immutable copy") {
        val state = baseState.copy(
            bag = List(Die(1, 6, BonusType.None, 0)),
            phase = Phase.Roll,
            usedCombinations = Set(Combination.Chance)
        )

        val updated = state.addMoney(12)

        assertEquals(updated.money, state.money + 12)
        assertEquals(updated.bag, state.bag)
        assertEquals(updated.phase, state.phase)
        assertEquals(updated.usedCombinations, state.usedCombinations)
    }

    test("RolledDie, Cupgrade and LockedRow should be constructible") {
        val die = Die(1, 6, BonusType.None, 0)
        val rolled = RolledDie(die, 3)
        val cupgrade = Cupgrade("+1 money", _.addMoney(1))
        val score = Score(3, 2)
        val row = LockedRow(List(rolled), Combination.Ones, score)
        val state = baseState

        assertEquals(rolled.die, die)
        assertEquals(rolled.value, 3)
        assertEquals(cupgrade.name, "+1 money")
        assertEquals(cupgrade.effect(state).money, state.money + 1)
        assertEquals(row.dice, List(rolled))
        assertEquals(row.combination, Combination.Ones)
        assertEquals(row.score.total, 6)
    }
}