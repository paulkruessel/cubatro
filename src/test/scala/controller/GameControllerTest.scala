import controller.internal_controller.*
import model.*
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import util.Observer

class GameControllerTest extends AnyWordSpec with Matchers:

  private val plainDie = Die(1, 6, BonusType.None, 0)
  private val chipDie = Die(1, 6, BonusType.Chips, 2)
  private val multDie = Die(1, 6, BonusType.Mult, 2)

  private def setState(controller: GameController, state: GameState): Unit =
    val field = classOf[GameController].getDeclaredField("currentState")
    field.setAccessible(true)
    field.set(controller, state)

  private def state(
      phase: Phase = Phase.Select,
      bag: List[Die] = List.fill(20)(plainDie),
      availableDice: List[Die] = List.fill(8)(plainDie),
      selectedDice: List[Die] = Nil,
      diceInPlay: List[RolledDie] = Nil,
      diceToRoll: List[RolledDie] = Nil,
      lockedRows: List[LockedRow] = Nil,
      cupgrades: List[Cupgrade] = Nil,
      discards: Int = 4,
      rerolls: Int = 4,
      totalRerolls: Int = 4,
      plays: Int = 6,
      targetScore: Int = 1000,
      score: Int = 0,
      maxAvailableDice: Int = 8
  ): GameState =
    GameState(
      bag = bag,
      availableDice = availableDice,
      maxAvailableDice = maxAvailableDice,
      selectedDice = selectedDice,
      diceInPlay = diceInPlay,
      diceToRoll = diceToRoll,
      lockedRows = lockedRows,
      cupgrades = cupgrades,
      discards = discards,
      rerolls = rerolls,
      totalRerolls = totalRerolls,
      plays = plays,
      targetScore = targetScore,
      score = score,
      phase = phase
    )

  private class CountingObserver extends Observer:
    var updates = 0
    override def update(): Unit = updates += 1

  "GameController" should {

    "initialize default state correctly" in {
      val controller = new GameController()
      val s = controller.state

      controller.isRunning shouldBe false
      s.phase shouldBe Phase.Draw
      s.availableDice shouldBe Nil
      s.selectedDice shouldBe Nil
      s.diceInPlay shouldBe Nil
      s.diceToRoll shouldBe Nil
      s.lockedRows shouldBe Nil
      s.discards shouldBe 4
      s.rerolls shouldBe 4
      s.totalRerolls shouldBe 4
      s.plays shouldBe 6
      s.targetScore shouldBe 1000
      s.score shouldBe 0
      s.bag.size shouldBe 24
    }

    "start and move from Draw to Select" in {
      val controller = new GameController()

      controller.start()

      controller.isRunning shouldBe true
      controller.state.phase shouldBe Phase.Select
      controller.state.availableDice.size shouldBe 8
      controller.state.bag.size shouldBe 16
    }

    "notify observers on start" in {
      val controller = new GameController()
      val observer = new CountingObserver

      controller.add(observer)
      controller.start()

      observer.updates shouldBe 1
    }

    "draw dice from bag into hand" in {
      val controller = new GameController()
      val initial = state(
        phase = Phase.Draw,
        bag = List.fill(10)(plainDie),
        availableDice = List.fill(3)(chipDie),
        maxAvailableDice = 8
      )

      val drawn = controller.drawDice(initial)

      drawn.availableDice.size shouldBe 8
      drawn.bag.size shouldBe 5
    }

    "not draw dice if hand is already full" in {
      val controller = new GameController()
      val initial = state(
        bag = List.fill(10)(plainDie),
        availableDice = List.fill(8)(chipDie)
      )

      val drawn = controller.drawDice(initial)

      drawn.availableDice.size shouldBe 8
      drawn.bag.size shouldBe 10
    }

    "select valid dice and keep order" in {
      val controller = new GameController()
      val initial = state(
        availableDice = List(plainDie, chipDie, multDie)
      )

      val selected = controller.selectDice(initial, List(2, 0))

      selected.selectedDice shouldBe List(multDie, plainDie)
      selected.availableDice shouldBe List(chipDie)
    }

    "ignore invalid selection indices" in {
      val controller = new GameController()
      val initial = state(availableDice = List(plainDie, chipDie))

      val selected = controller.selectDice(initial, List(-1, 5))

      selected shouldBe initial
    }

    "return the same state instance when selection limit is reached" in {
      val controller = new GameController()
      val initial = state(
        availableDice = List(plainDie, chipDie, multDie),
        selectedDice = List.fill(5)(plainDie)
      )

      controller.selectDice(initial, List(0)) shouldBe theSameInstanceAs(initial)
    }

    "limit selected dice to five" in {
      val controller = new GameController()
      val initial = state(availableDice = List.fill(8)(plainDie))

      val selected = controller.selectDice(initial, List(0, 1, 2, 3, 4, 5, 6))

      selected.selectedDice.size shouldBe 5
      selected.availableDice.size shouldBe 3
    }

    "handle Select command in Select phase" in {
      val controller = new GameController()
      controller.start()

      val result = controller.handle(GameCommand.Select(List(0, 1)))

      result.isRight shouldBe true
      controller.state.selectedDice.size shouldBe 2
      controller.state.availableDice.size shouldBe 6
    }

    "discard selected dice and reduce discards" in {
      val controller = new GameController()
      val initial = state(
        selectedDice = List(plainDie, chipDie),
        availableDice = List.fill(6)(plainDie),
        bag = List.fill(10)(plainDie),
        discards = 4
      )

      val discarded = controller.discardDice(initial)

      discarded.selectedDice shouldBe Nil
      discarded.discards shouldBe 3
      discarded.availableDice.size shouldBe 8
    }

    "not discard without selected dice or without discards" in {
      val controller = new GameController()
      val withoutSelected = state(selectedDice = Nil, discards = 4)
      val withoutDiscards = state(selectedDice = List(plainDie), discards = 0)
      val negativeDiscards = state(selectedDice = List(plainDie), discards = -1)

      controller.discardDice(withoutSelected) shouldBe withoutSelected
      controller.discardDice(withoutDiscards) shouldBe withoutDiscards
      controller.discardDice(negativeDiscards) shouldBe theSameInstanceAs(negativeDiscards)
    }

    "handle discard command in Select phase" in {
      val controller = new GameController()
      controller.start()
      controller.handle(GameCommand.Select(List(0)))

      val result = controller.handle(GameCommand.Discard)

      result.isRight shouldBe true
      controller.state.selectedDice shouldBe Nil
      controller.state.discards shouldBe 3
    }

    "add selected dice to play" in {
      val controller = new GameController()
      val initial = state(
        selectedDice = List(plainDie, chipDie),
        availableDice = List.fill(6)(plainDie),
        bag = List.fill(10)(plainDie)
      )

      val played = controller.addDiceToPlay(initial)

      played.selectedDice shouldBe Nil
      played.diceInPlay.size shouldBe 2
      played.availableDice.size shouldBe 8
      played.diceInPlay.foreach(_.value should (be >= 1 and be <= 6))
    }

    "not add dice to play if no dice are selected" in {
      val controller = new GameController()
      val initial = state(selectedDice = Nil)

      controller.addDiceToPlay(initial) shouldBe theSameInstanceAs(initial)
    }

    "handle PlaySelected and advance to PickOut" in {
      val controller = new GameController()
      controller.start()
      controller.handle(GameCommand.Select(List(0)))

      val result = controller.handle(GameCommand.PlaySelected)

      result.isRight shouldBe true
      controller.state.phase shouldBe Phase.PickOut
      controller.state.selectedDice shouldBe Nil
      controller.state.diceInPlay.size shouldBe 1
    }

    "reject wrong command in Select phase" in {
      val controller = new GameController()
      controller.start()

      controller.handle(GameCommand.Reroll) shouldBe
        Left("Allowed: select, discard, play, help, quit, undo, redo")
    }

    "reject invalid command in Select phase" in {
      val controller = new GameController()
      controller.start()

      controller.handle(GameCommand.Invalid) shouldBe
        Left("Unknown command. Use help to see valid commands.")
    }

    "select played dice for reroll" in {
      val controller = new GameController()
      val rolled1 = RolledDie(plainDie, 1)
      val rolled2 = RolledDie(chipDie, 2)
      val rolled3 = RolledDie(multDie, 3)
      val initial = state(diceInPlay = List(rolled1, rolled2, rolled3))

      val picked = controller.selectPlayedDice(initial, List(2, 0))

      picked.diceToRoll shouldBe List(rolled3, rolled1)
      picked.diceInPlay shouldBe List(rolled2)
    }

    "ignore invalid played dice indices" in {
      val controller = new GameController()
      val initial = state(diceInPlay = List(RolledDie(plainDie, 1)))

      controller.selectPlayedDice(initial, List(-1, 5)) shouldBe initial
    }

    "return the same state instance when played dice indices are invalid" in {
      val controller = new GameController()
      val initial = state(diceInPlay = List(RolledDie(plainDie, 1)))

      controller.selectPlayedDice(initial, List(-1, 5)) shouldBe theSameInstanceAs(initial)
    }

    "roll selected dice and reduce rerolls" in {
      val controller = new GameController()
      val initial = state(
        diceInPlay = List(RolledDie(plainDie, 1)),
        diceToRoll = List(RolledDie(chipDie, 2)),
        rerolls = 4
      )

      val rolled = controller.rollDice(initial)

      rolled.diceToRoll shouldBe Nil
      rolled.diceInPlay.size shouldBe 2
      rolled.rerolls shouldBe 3
    }

    "not roll without rerolls or without dice to roll" in {
      val controller = new GameController()
      val withoutRerolls = state(diceToRoll = List(RolledDie(plainDie, 1)), rerolls = 0)
      val withoutDice = state(diceToRoll = Nil, rerolls = 4)
      val negativeRerolls = state(diceToRoll = List(RolledDie(plainDie, 1)), rerolls = -1)

      controller.rollDice(withoutRerolls) shouldBe withoutRerolls
      controller.rollDice(withoutDice) shouldBe withoutDice
      controller.rollDice(negativeRerolls) shouldBe theSameInstanceAs(negativeRerolls)
    }

    "handle pick and reroll in PickOut phase" in {
      val controller = new GameController()
      setState(
        controller,
        state(
          phase = Phase.PickOut,
          diceInPlay = List(RolledDie(plainDie, 1), RolledDie(chipDie, 2)),
          rerolls = 4
        )
      )

      controller.handle(GameCommand.Pick(List(0))).isRight shouldBe true
      controller.state.diceToRoll.size shouldBe 1

      controller.handle(GameCommand.Reroll).isRight shouldBe true
      controller.state.phase shouldBe Phase.PickOut
      controller.state.rerolls shouldBe 3
      controller.state.diceToRoll shouldBe Nil
    }

    "score dice in PickOut phase and continue game" in {
      val controller = new GameController()
      setState(
        controller,
        state(
          phase = Phase.PickOut,
          diceInPlay = List.fill(5)(RolledDie(plainDie, 6)),
          targetScore = 10000,
          plays = 6
        )
      )

      val result = controller.handle(GameCommand.ScoreCurrent)

      result.isRight shouldBe true
      controller.state.score should be > 0
      controller.state.lockedRows.size shouldBe 1
      controller.state.plays shouldBe 5
      controller.state.phase shouldBe Phase.Select
    }

    "score dice in Score phase" in {
      val controller = new GameController()
      setState(
        controller,
        state(
          phase = Phase.Score,
          diceInPlay = List.fill(5)(RolledDie(plainDie, 6)),
          targetScore = 10000,
          plays = 6
        )
      )

      controller.handle(GameCommand.ScoreCurrent).isRight shouldBe true

      controller.state.score should be > 0
      controller.state.lockedRows.size shouldBe 1
      controller.state.phase shouldBe Phase.Select
    }

    "reject invalid command in PickOut phase" in {
      val controller = new GameController()
      setState(controller, state(phase = Phase.PickOut))

      controller.handle(GameCommand.Invalid) shouldBe
        Left("Unknown command. Use help to see valid commands.")
    }

    "reject wrong command in PickOut phase" in {
      val controller = new GameController()
      setState(controller, state(phase = Phase.PickOut))

      controller.handle(GameCommand.Discard) shouldBe
        Left("Allowed: pick, reroll, score, help, quit, undo, redo")
    }

    "reject invalid command in Score phase" in {
      val controller = new GameController()
      setState(controller, state(phase = Phase.Score))

      controller.handle(GameCommand.Invalid) shouldBe
        Left("Unknown command. Use help to see valid commands.")
    }

    "reject wrong command in Score phase" in {
      val controller = new GameController()
      setState(controller, state(phase = Phase.Score))

      controller.handle(GameCommand.Select(List(0))) shouldBe
        Left("Allowed: score, help, quit, undo, redo")
    }

    "score dice and apply cupgrade effect" in {
      val controller = new GameController()
      val upgrade = Cupgrade("Bonus", s => s.copy(score = s.score + 100))

      val initial = state(
        diceInPlay = List(RolledDie(plainDie, 6)),
        cupgrades = List(upgrade)
      )

      val scored = controller.scoreDiceInPlay(initial)

      scored.score should be >= 100
    }

    "not score empty dice list" in {
      val controller = new GameController()
      val initial = state(diceInPlay = Nil)

      controller.scoreDiceInPlay(initial) shouldBe initial
    }

    "advance to Win after scoring enough points" in {
      val controller = new GameController()
      setState(
        controller,
        state(
          phase = Phase.PickOut,
          diceInPlay = List.fill(5)(RolledDie(plainDie, 6)),
          targetScore = 1,
          plays = 6
        )
      )

      controller.handle(GameCommand.ScoreCurrent).isRight shouldBe true

      controller.state.phase shouldBe Phase.Win
      controller.viewState.isWin shouldBe true
    }

    "advance to Win when score matches target exactly" in {
      val controller = new GameController()
      val initial = state(
        phase = Phase.PickOut,
        diceInPlay = List.fill(5)(RolledDie(plainDie, 6)),
        targetScore = 9999,
        plays = 6
      )
      val expectedScore = controller.scoreDiceInPlay(initial).score

      setState(controller, initial.copy(targetScore = expectedScore))

      controller.handle(GameCommand.ScoreCurrent).isRight shouldBe true

      controller.state.phase shouldBe Phase.Win
    }

    "advance to Lose when no plays remain" in {
      val controller = new GameController()
      setState(
        controller,
        state(
          phase = Phase.PickOut,
          diceInPlay = List(RolledDie(plainDie, 1)),
          targetScore = 1000,
          plays = 1
        )
      )

      controller.handle(GameCommand.ScoreCurrent).isRight shouldBe true

      controller.state.phase shouldBe Phase.Lose
      controller.viewState.isLose shouldBe true
    }

    "advance to Lose when plays drop below zero" in {
      val controller = new GameController()
      setState(
        controller,
        state(
          phase = Phase.EndEval,
          score = 0,
          targetScore = 1000,
          plays = -1
        )
      )

      controller.start()

      controller.state.phase shouldBe Phase.Lose
    }

    "move Roll phase to Score when rerolls are zero" in {
      val controller = new GameController()
      setState(
        controller,
        state(
          phase = Phase.Roll,
          diceInPlay = List(RolledDie(plainDie, 1)),
          diceToRoll = List(RolledDie(plainDie, 2)),
          rerolls = 0
        )
      )

      controller.handle(GameCommand.Undo) shouldBe Left("Nothing to undo")
      controller.start()

      controller.state.phase should (be(Phase.Select) or be(Phase.Score))
    }

    "move Roll phase to Score when rerolls are negative" in {
      val controller = new GameController()
      setState(
        controller,
        state(
          phase = Phase.Roll,
          diceInPlay = List(RolledDie(plainDie, 1)),
          diceToRoll = List(RolledDie(plainDie, 2)),
          rerolls = -1
        )
      )

      controller.start()

      controller.state.phase shouldBe Phase.Score
    }

    "reject command in automatic phase" in {
      val controller = new GameController()
      setState(controller, state(phase = Phase.Draw))

      controller.handle(GameCommand.Select(List(0))) shouldBe
        Left("No command allowed in phase Draw")
    }

    "create complete view state" in {
      val controller = new GameController()
      val row = LockedRow(List(RolledDie(plainDie, 6)), Combination.Sixes, 43)

      setState(
        controller,
        state(
          phase = Phase.Select,
          availableDice = List(plainDie, chipDie, multDie),
          selectedDice = List(chipDie),
          diceInPlay = List(RolledDie(plainDie, 5)),
          diceToRoll = List(RolledDie(plainDie, 3)),
          lockedRows = List(row),
          score = 43
        )
      )

      val view = controller.viewState

      view.targetScore shouldBe 1000
      view.score shouldBe 43
      view.phase shouldBe "Select"
      view.hand should contain("0:[d1-6]")
      view.hand should contain("1:[d1-6:+2C]")
      view.hand should contain("2:[d1-6:+2M]")
      view.selected should contain("0:[d1-6:+2C]")
      view.inPlay should contain("0:[5]")
      view.toRoll should contain("0:[3]")
      view.lockedRows.head should include("Sixes")
      view.isWin shouldBe false
      view.isLose shouldBe false
    }

    "include bonus metadata for dice in every view section" in {
      val controller = new GameController()

      setState(
        controller,
        state(
          availableDice = List(chipDie),
          selectedDice = List(multDie),
          diceInPlay = List(RolledDie(chipDie, 5)),
          diceToRoll = List(RolledDie(multDie, 3))
        )
      )

      val view = controller.viewState

      view.hand shouldBe List("0:[d1-6:+2C]")
      view.selected shouldBe List("0:[d1-6:+2M]")
      view.inPlay shouldBe List("0:[5:+2C]")
      view.toRoll shouldBe List("0:[3:+2M]")

      view.handDice.head.bonusType shouldBe BonusType.Chips
      view.handDice.head.bonusValue shouldBe 2
      view.handDice.head.guiText shouldBe "[d1-6:+2C]"
      view.handDice.head.tooltip shouldBe "Bonus: +2 Chips"

      view.selectedDiceViews.head.bonusType shouldBe BonusType.Mult
      view.selectedDiceViews.head.guiText shouldBe "[d1-6:+2M]"
      view.selectedDiceViews.head.tooltip shouldBe "Bonus: +2 Mult"

      view.inPlayDice.head.text shouldBe "0:[5:+2C]"
      view.inPlayDice.head.guiText shouldBe "[5]"
      view.toRollDice.head.text shouldBe "0:[3:+2M]"
      view.toRollDice.head.guiText shouldBe "[3]"
    }

    "describe dice without a bonus explicitly in tooltips" in {
      val dieView = DieView("0:[1]", BonusType.None, 0)

      dieView.guiText shouldBe "[1]"
      dieView.tooltip shouldBe "Bonus: none"
    }

    "undo after select restores previous state" in {
      val controller = new GameController()
      controller.start()

      controller.handle(GameCommand.Select(List(0)))
      controller.state.selectedDice.size shouldBe 1

      controller.handle(GameCommand.Undo).isRight shouldBe true

      controller.state.selectedDice shouldBe Nil
      controller.state.availableDice.size shouldBe 8
    }

    "redo after undo reapplies previous command" in {
      val controller = new GameController()
      controller.start()

      controller.handle(GameCommand.Select(List(0)))
      controller.handle(GameCommand.Undo)

      controller.handle(GameCommand.Redo).isRight shouldBe true

      controller.state.selectedDice.size shouldBe 1
      controller.state.availableDice.size shouldBe 7
    }

    "return errors when undo or redo is not possible" in {
      val controller = new GameController()

      controller.handle(GameCommand.Undo) shouldBe Left("Nothing to undo")
      controller.handle(GameCommand.Redo) shouldBe Left("Nothing to redo")
    }

    "not store unchanged commands in undo history" in {
      val controller = new GameController()
      controller.start()

      controller.handle(GameCommand.Discard).isRight shouldBe true

      controller.handle(GameCommand.Undo) shouldBe Left("Nothing to undo")
    }

    "set isRunning to false when Quit is handled" in {
      val controller = new GameController()
      controller.start()

      controller.isRunning shouldBe true

      controller.handle(GameCommand.Quit) shouldBe Left("Game ended by player.")

      controller.isRunning shouldBe false
    }

    "notify observers when Quit is handled" in {
      val controller = new GameController()
      val observer = new CountingObserver

      controller.add(observer)
      controller.start()
      val updatesAfterStart = observer.updates

      controller.handle(GameCommand.Quit)

      observer.updates shouldBe updatesAfterStart + 1
    }

    "not select index equal to available dice length" in {
      val controller = new GameController()
      val initial = state(availableDice = List(plainDie, chipDie))

      val selected = controller.selectDice(initial, List(2))

      selected shouldBe initial
    }

    "not pick index equal to dice in play length" in {
      val controller = new GameController()
      val initial = state(
        diceInPlay = List(RolledDie(plainDie, 1), RolledDie(chipDie, 2))
      )

      val picked = controller.selectPlayedDice(initial, List(2))

      picked shouldBe initial
    }

    "discard when exactly one discard remains" in {
      val controller = new GameController()
      val initial = state(
        selectedDice = List(plainDie),
        availableDice = List.fill(7)(plainDie),
        bag = List.fill(5)(plainDie),
        discards = 1
      )

      val discarded = controller.discardDice(initial)

      discarded.discards shouldBe 0
      discarded.selectedDice shouldBe Nil
    }

    "reroll when exactly one reroll remains" in {
      val controller = new GameController()
      val initial = state(
        diceInPlay = Nil,
        diceToRoll = List(RolledDie(plainDie, 3)),
        rerolls = 1
      )

      val rolled = controller.rollDice(initial)

      rolled.rerolls shouldBe 0
      rolled.diceToRoll shouldBe Nil
      rolled.diceInPlay.size shouldBe 1
    }
  }
