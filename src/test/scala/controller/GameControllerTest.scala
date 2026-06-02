import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import controller.*
import model.*

class GameControllerTest extends AnyWordSpec with Matchers {

  private val plain = Die(bonusType = BonusType.None, bonusValue = 0)
  private val fixed = Die(min = 4, max = 4, bonusType = BonusType.None, bonusValue = 0)
  private val chip = Die(bonusType = BonusType.Chips, bonusValue = 2)
  private val mult = Die(bonusType = BonusType.Mult, bonusValue = 1)

  private def setState(controller: GameController, state: GameState): Unit =
    val field = classOf[GameController].getDeclaredField("currentState")
    field.setAccessible(true)
    field.set(controller, state)

  private def baseState(phase: Phase): GameState =
    GameState(
      bag = List.fill(10)(plain),
      availableDice = List.fill(8)(plain),
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
      phase = phase
    )

  "GameController" should {

    "initialize default state correctly" in {
      val controller = new GameController()

      controller.state.score shouldBe 0
      controller.state.phase shouldBe Phase.Draw
      controller.state.targetScore shouldBe 1000
      controller.state.discards shouldBe 4
      controller.state.rerolls shouldBe 4
      controller.state.totalRerolls shouldBe 4
      controller.state.plays shouldBe 6
      controller.state.bag.length shouldBe 24
    }

    "draw dice from bag into hand and remove them from bag" in {
      val controller = new GameController()
      val state = baseState(Phase.Draw).copy(
        bag = List(plain, chip, mult),
        availableDice = Nil,
        maxAvailableDice = 2
      )

      val drawn = controller.drawDice(state)

      drawn.availableDice.length shouldBe 2
      drawn.bag.length shouldBe 1
      drawn.availableDice.toSet.subsetOf(Set(plain, chip, mult)) shouldBe true
      drawn.bag.toSet.subsetOf(Set(plain, chip, mult)) shouldBe true
    }

    "not draw dice if hand is already full" in {
      val controller = new GameController()
      val state = baseState(Phase.Draw).copy(
        bag = List(chip, mult),
        availableDice = List(plain, plain),
        maxAvailableDice = 2
      )

      val drawn = controller.drawDice(state)

      drawn.availableDice shouldBe state.availableDice
      drawn.bag.length shouldBe state.bag.length
      drawn.bag.toSet shouldBe state.bag.toSet
    }

    "start and move from Draw to Select" in {
      val controller = new GameController()

      controller.start()

      controller.state.phase shouldBe Phase.Select
      controller.state.availableDice.length shouldBe 8
    }

    "select valid dice and keep order" in {
      val controller = new GameController()
      val state = baseState(Phase.Select).copy(
        availableDice = List(plain, chip, mult)
      )

      val selected = controller.selectDice(state, List(0, 2))

      selected.selectedDice shouldBe List(plain, mult)
      selected.availableDice shouldBe List(chip)
    }

    "ignore invalid selection indices and return same instance" in {
      val controller = new GameController()
      val state = baseState(Phase.Select).copy(
        availableDice = List(plain, chip, mult)
      )

      noException should be thrownBy controller.selectDice(state, List(3))
      controller.selectDice(state, List(-1, 3, 99)) should be theSameInstanceAs state
    }

    "limit selected dice to five" in {
      val controller = new GameController()
      val state = baseState(Phase.Select).copy(
        availableDice = List.fill(8)(plain),
        selectedDice = List.fill(4)(chip)
      )

      val selected = controller.selectDice(state, List(0, 1, 2, 3))

      selected.selectedDice.length shouldBe 5
      selected.availableDice.length shouldBe 7
    }

    "handle select command in Select phase" in {
      val controller = new GameController()
      controller.start()

      val result = controller.handle(GameCommand.Select(List(0, 1, 2)))

      result.isRight shouldBe true
      controller.state.selectedDice.length shouldBe 3
      controller.state.availableDice.length shouldBe 5
    }

    "discard selected dice and reduce discards" in {
      val controller = new GameController()
      val state = baseState(Phase.Select).copy(
        selectedDice = List(plain),
        availableDice = Nil,
        bag = List(chip, mult),
        discards = 4
      )

      val discarded = controller.discardDice(state)

      discarded.discards shouldBe 3
      discarded.selectedDice shouldBe Nil
      discarded.availableDice.length shouldBe 2
    }

    "not discard without selected dice or without discards" in {
      val controller = new GameController()
      val noSelection = baseState(Phase.Select)
      val noDiscards = baseState(Phase.Select).copy(selectedDice = List(plain), discards = 0)

      controller.discardDice(noSelection) should be theSameInstanceAs noSelection
      controller.discardDice(noDiscards) should be theSameInstanceAs noDiscards
    }

    "handle discard command in Select phase" in {
      val controller = new GameController()
      controller.start()
      controller.handle(GameCommand.Select(List(0)))

      val result = controller.handle(GameCommand.Discard)

      result.isRight shouldBe true
      controller.state.discards shouldBe 3
      controller.state.selectedDice shouldBe Nil
    }

    "add selected dice to play" in {
      val controller = new GameController()
      val state = baseState(Phase.Select).copy(
        selectedDice = List(fixed, fixed),
        availableDice = Nil,
        bag = Nil
      )

      val played = controller.addDiceToPlay(state)

      played.selectedDice shouldBe Nil
      played.diceInPlay.map(_.value) shouldBe List(4, 4)
    }

    "not add dice to play if no dice are selected and return same instance" in {
      val controller = new GameController()
      val state = baseState(Phase.Select).copy(
        bag = List(chip, mult),
        availableDice = List(plain, plain),
        maxAvailableDice = 2,
        selectedDice = Nil
      )

      controller.addDiceToPlay(state) should be theSameInstanceAs state
    }

    "handle play selected and move through Roll to PickOut" in {
      val controller = new GameController()
      val state = baseState(Phase.Select).copy(
        selectedDice = List(fixed),
        availableDice = Nil,
        bag = Nil
      )
      setState(controller, state)

      val result = controller.handle(GameCommand.PlaySelected)

      result.isRight shouldBe true
      controller.state.phase shouldBe Phase.PickOut
      controller.state.diceInPlay.map(_.value) shouldBe List(4)
    }

    "reject wrong command in Select phase with exact message" in {
      val controller = new GameController()
      controller.start()

      controller.handle(GameCommand.Reroll) shouldBe Left("Allowed: select, discard, play, help, quit, undo, redo")
    }

    "reject invalid command in Select phase with exact message" in {
    val controller = new GameController()
    setState(controller, baseState(Phase.Select))

    controller.handle(GameCommand.Invalid) shouldBe
      Left("Unknown command. Use help to see valid commands.")
  }

    "select played dice for reroll" in {
      val controller = new GameController()
      val state = baseState(Phase.PickOut).copy(
        diceInPlay = List(RolledDie(plain, 1), RolledDie(chip, 2), RolledDie(mult, 3))
      )

      val selected = controller.selectPlayedDice(state, List(0, 2))

      selected.diceInPlay shouldBe List(RolledDie(chip, 2))
      selected.diceToRoll shouldBe List(RolledDie(plain, 1), RolledDie(mult, 3))
    }

    "ignore invalid played dice indices and return same instance" in {
      val controller = new GameController()
      val state = baseState(Phase.PickOut).copy(
        diceInPlay = List(RolledDie(plain, 1))
      )

      noException should be thrownBy controller.selectPlayedDice(state, List(1))
      controller.selectPlayedDice(state, List(-1, 1, 99)) should be theSameInstanceAs state
    }

    "roll selected dice and reduce rerolls" in {
      val controller = new GameController()
      val state = baseState(Phase.Roll).copy(
        diceInPlay = List(RolledDie(plain, 1)),
        diceToRoll = List(RolledDie(fixed, 2)),
        rerolls = 2
      )

      val rolled = controller.rollDice(state)

      rolled.diceInPlay.map(_.value) shouldBe List(1, 4)
      rolled.diceToRoll shouldBe Nil
      rolled.rerolls shouldBe 1
    }

    "not roll without rerolls or without dice to roll" in {
      val controller = new GameController()
      val noRerolls = baseState(Phase.Roll).copy(
        rerolls = 0,
        diceToRoll = List(RolledDie(fixed, 1))
      )
      val noDiceToRoll = baseState(Phase.Roll).copy(
        rerolls = 2,
        diceToRoll = Nil
      )

      controller.rollDice(noRerolls) shouldBe noRerolls
      controller.rollDice(noDiceToRoll) shouldBe noDiceToRoll
    }

    "handle pick and reroll in PickOut phase" in {
      val controller = new GameController()
      val state = baseState(Phase.PickOut).copy(
        diceInPlay = List(RolledDie(fixed, 4)),
        rerolls = 2
      )
      setState(controller, state)

      controller.handle(GameCommand.Pick(List(0))).isRight shouldBe true
      controller.state.diceToRoll.length shouldBe 1

      controller.handle(GameCommand.Reroll).isRight shouldBe true
      controller.state.phase shouldBe Phase.PickOut
      controller.state.diceToRoll shouldBe Nil
      controller.state.rerolls shouldBe 1
    }

    "score dice in PickOut phase" in {
      val controller = new GameController()
      val state = baseState(Phase.PickOut).copy(
        diceInPlay = List(RolledDie(plain, 6), RolledDie(plain, 6)),
        plays = 6
      )
      setState(controller, state)

      val result = controller.handle(GameCommand.ScoreCurrent)

      result.isRight shouldBe true
      controller.state.lockedRows.length shouldBe 1
      controller.state.plays shouldBe 5
    }

    "reject invalid command in PickOut phase with exact message" in {
      val controller = new GameController()
      setState(controller, baseState(Phase.PickOut))

      controller.handle(GameCommand.Discard) shouldBe Left("Allowed: pick, reroll, score, help, quit, undo, redo")
    }

    "score dice in Score phase" in {
      val controller = new GameController()
      val state = baseState(Phase.Score).copy(
        diceInPlay = List(RolledDie(plain, 6), RolledDie(plain, 6)),
        plays = 6
      )
      setState(controller, state)

      val result = controller.handle(GameCommand.ScoreCurrent)

      result.isRight shouldBe true
      controller.state.lockedRows.length shouldBe 1
      controller.state.plays shouldBe 5
    }

    "reject wrong command in Score phase with exact message" in {
      val controller = new GameController()
      setState(controller, baseState(Phase.Score))

      controller.handle(GameCommand.PlaySelected) shouldBe Left("Allowed: score, help, quit, undo, redo")
    }

    "score dice and apply cupgrade effect" in {
      val controller = new GameController()
      val state = baseState(Phase.Score).copy(
        diceInPlay = List(RolledDie(plain, 6), RolledDie(plain, 6)),
        cupgrades = List(Cupgrade("bonus", s => s.copy(score = s.score + 10)))
      )

      val scored = controller.scoreDiceInPlay(state)

      scored.score should be > 10
      scored.lockedRows.length shouldBe 1
      scored.diceInPlay shouldBe Nil
      scored.diceToRoll shouldBe Nil
      scored.rerolls shouldBe state.totalRerolls
    }

    "not score empty dice list" in {
      val controller = new GameController()
      val state = baseState(Phase.Score)

      controller.scoreDiceInPlay(state) shouldBe state
    }

    "move Roll phase to Score when rerolls are zero" in {
      val controller = new GameController()
      val state = baseState(Phase.Roll).copy(
        rerolls = 0,
        diceToRoll = List(RolledDie(fixed, 2))
      )
      setState(controller, state)

      controller.start()

      controller.state.phase shouldBe Phase.Score
    }

    "move Roll phase to PickOut when rerolls remain" in {
      val controller = new GameController()
      val state = baseState(Phase.Roll).copy(
        rerolls = 2,
        diceToRoll = List(RolledDie(fixed, 2))
      )
      setState(controller, state)

      controller.start()

      controller.state.phase shouldBe Phase.PickOut
      controller.state.rerolls shouldBe 1
    }

    "move EndEval phase to Win" in {
      val controller = new GameController()
      setState(controller, baseState(Phase.EndEval).copy(score = 1000, targetScore = 1000, plays = 1))

      controller.start()

      controller.state.phase shouldBe Phase.Win
    }

    "move EndEval phase to Lose" in {
      val controller = new GameController()
      setState(controller, baseState(Phase.EndEval).copy(score = 0, targetScore = 1000, plays = 0))

      controller.start()

      controller.state.phase shouldBe Phase.Lose
    }

    "move EndEval phase to Draw when game continues" in {
      val controller = new GameController()
      setState(controller, baseState(Phase.EndEval).copy(score = 10, targetScore = 1000, plays = 1))

      controller.start()

      controller.state.phase shouldBe Phase.Select
    }

    "reject command in automatic phase with exact message" in {
      val controller = new GameController()
      setState(controller, baseState(Phase.Draw))

      controller.handle(GameCommand.Help) shouldBe Left("No command allowed in phase Draw")
    }

    "create complete view state" in {
      val controller = new GameController()
      val state = baseState(Phase.Select).copy(
        availableDice = List(plain, chip, mult),
        selectedDice = List(chip),
        diceInPlay = List(RolledDie(plain, 5)),
        diceToRoll = List(RolledDie(plain, 2)),
        lockedRows = List(
          LockedRow(
            dice = List(RolledDie(plain, 6)),
            combination = Combination.Sixes,
            score = 43
          )
        ),
        score = 100,
        targetScore = 1000,
        plays = 3,
        rerolls = 2,
        discards = 1
      )
      setState(controller, state)

      val viewState = controller.viewState

      viewState.targetScore shouldBe 1000
      viewState.score shouldBe 100
      viewState.phase shouldBe "Select"
      viewState.plays shouldBe 3
      viewState.rerolls shouldBe 2
      viewState.discards shouldBe 1
      viewState.hand shouldBe List("0:[d1-6]", "1:[d1-6:+2C]", "2:[d1-6:+1M]")
      viewState.selected shouldBe List("0:[d1-6:+2C]")
      viewState.inPlay shouldBe List("0:[5]")
      viewState.toRoll shouldBe List("0:[2]")
      viewState.lockedRows shouldBe List("1. Sixes -> 43")
      viewState.isWin shouldBe false
      viewState.isLose shouldBe false
    }

    "not discard when discards are negative" in {
      val controller = new GameController()
      val state = baseState(Phase.Select).copy(
        selectedDice = List(plain),
        discards = -1
      )

      controller.discardDice(state) shouldBe state
    }

    "not roll when rerolls are negative" in {
      val controller = new GameController()
      val state = baseState(Phase.Roll).copy(
        rerolls = -1,
        diceToRoll = List(RolledDie(fixed, 1))
      )

      controller.rollDice(state) shouldBe state
    }

    "move Roll phase with negative rerolls to Score" in {
      val controller = new GameController()
      val state = baseState(Phase.Roll).copy(
        rerolls = -1,
        diceToRoll = List(RolledDie(fixed, 2))
      )
      setState(controller, state)

      controller.start()

      controller.state.phase shouldBe Phase.Score
    }

    "move EndEval phase with negative plays to Lose" in {
      val controller = new GameController()
      setState(controller, baseState(Phase.EndEval).copy(score = 0, targetScore = 1000, plays = -1))

      controller.start()

      controller.state.phase shouldBe Phase.Lose
    }

    "undo after select restores previous state" in {
      val controller = new GameController()
      val state = baseState(Phase.Select).copy(
        availableDice = List(plain, chip, mult),
        selectedDice = Nil
      )
      setState(controller, state)

      controller.handle(GameCommand.Select(List(0, 1)))
      controller.state.selectedDice shouldBe List(plain, chip)

      controller.handle(GameCommand.Undo) shouldBe Right(state)
      controller.state shouldBe state
    }

    "redo after undo reapplies previous command" in {
      val controller = new GameController()
      val state = baseState(Phase.Select).copy(
        availableDice = List(plain, chip, mult),
        selectedDice = Nil
      )
      setState(controller, state)

      val afterSelect = controller.handle(GameCommand.Select(List(0, 1))).toOption.get
      controller.handle(GameCommand.Undo)

      controller.handle(GameCommand.Redo) shouldBe Right(afterSelect)
      controller.state shouldBe afterSelect
    }

    "return errors when undo or redo is not possible" in {
      val controller = new GameController()
      setState(controller, baseState(Phase.Select))

      controller.handle(GameCommand.Undo) shouldBe Left("Nothing to undo")
      controller.handle(GameCommand.Redo) shouldBe Left("Nothing to redo")
    }


    "reject invalid command object in PickOut phase with exact unknown-command message" in {
      val controller = new GameController()
      setState(controller, baseState(Phase.PickOut))

      controller.handle(GameCommand.Invalid) shouldBe
        Left("Unknown command. Use help to see valid commands.")
    }

    "reject invalid command object in Score phase with exact unknown-command message" in {
      val controller = new GameController()
      setState(controller, baseState(Phase.Score))

      controller.handle(GameCommand.Invalid) shouldBe
        Left("Unknown command. Use help to see valid commands.")
    }

    "not store unchanged commands in undo history" in {
      val controller = new GameController()
      val state = baseState(Phase.Select).copy(
        availableDice = List(plain, chip, mult),
        selectedDice = Nil
      )
      setState(controller, state)

      controller.handle(GameCommand.Select(List(99))) shouldBe Right(state)
      controller.state shouldBe state
      controller.handle(GameCommand.Undo) shouldBe Left("Nothing to undo")
      controller.state shouldBe state
    }

    "mark view state as win and lose" in {
      val controller = new GameController()

      setState(controller, baseState(Phase.Win))
      controller.viewState.isWin shouldBe true
      controller.viewState.isLose shouldBe false
      controller.viewState.phase shouldBe "Win"

      setState(controller, baseState(Phase.Lose))
      controller.viewState.isWin shouldBe false
      controller.viewState.isLose shouldBe true
      controller.viewState.phase shouldBe "Lose"
    }
  }
}
