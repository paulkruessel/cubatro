import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import controller.*
import model.*

class GameControllerTest extends AnyWordSpec with Matchers {

  private val plain = Die(bonusType = BonusType.None, bonusValue = 0)

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
    }

    "start and move from Draw to Select" in {
      val controller = new GameController()
      controller.start()
      controller.state.phase shouldBe Phase.Select
      controller.state.availableDice.length shouldBe 8
    }

    "select dice in select phase" in {
      val controller = new GameController()
      controller.start()
      controller.handle(GameCommand.Select(List(0, 1, 2))).isRight shouldBe true
      controller.state.selectedDice.length shouldBe 3
    }

    "play selected dice and move to PickOut" in {
      val controller = new GameController()
      controller.start()
      controller.handle(GameCommand.Select(List(0, 1, 2)))
      controller.handle(GameCommand.PlaySelected).isRight shouldBe true
      controller.state.phase shouldBe Phase.PickOut
    }

    "score dice in PickOut phase" in {
      val controller = new GameController()
      controller.start()
      controller.handle(GameCommand.Select(List(0, 1, 2)))
      controller.handle(GameCommand.PlaySelected)
      controller.handle(GameCommand.ScoreCurrent).isRight shouldBe true
      controller.state.lockedRows.length shouldBe 1
    }

    "discard selected dice" in {
      val controller = new GameController()
      controller.start()
      controller.handle(GameCommand.Select(List(0)))
      controller.handle(GameCommand.Discard).isRight shouldBe true
      controller.state.discards shouldBe 3
    }

    "reject wrong command in Select phase" in {
      val controller = new GameController()
      controller.start()
      controller.handle(GameCommand.Reroll).isLeft shouldBe true
    }

    "pick and reroll dice in PickOut phase" in {
      val controller = new GameController()
      controller.start()
      controller.handle(GameCommand.Select(List(0, 1, 2)))
      controller.handle(GameCommand.PlaySelected)
      controller.handle(GameCommand.Pick(List(0))).isRight shouldBe true
      controller.handle(GameCommand.Reroll).isRight shouldBe true
    }

    "reject invalid command in PickOut phase" in {
      val controller = new GameController()
      controller.start()
      controller.handle(GameCommand.Select(List(0, 1, 2)))
      controller.handle(GameCommand.PlaySelected)
      controller.handle(GameCommand.Discard).isLeft shouldBe true
    }

    "score in Score phase" in {
      val controller = new GameController()
      val state = baseState(Phase.Score).copy(
        diceInPlay = List(RolledDie(plain, 6), RolledDie(plain, 6))
      )
      setState(controller, state)

      controller.handle(GameCommand.ScoreCurrent).isRight shouldBe true
    }

    "reject wrong command in Score phase" in {
      val controller = new GameController()
      setState(controller, baseState(Phase.Score))

      controller.handle(GameCommand.PlaySelected).isLeft shouldBe true
    }

    "move Roll phase to Score when rerolls are zero" in {
      val controller = new GameController()
      val state = baseState(Phase.Roll).copy(
        rerolls = 0,
        diceToRoll = List(RolledDie(plain, 2))
      )
      setState(controller, state)

      controller.start()
      controller.state.phase shouldBe Phase.Score
    }

    "move EndEval phase to Win" in {
      val controller = new GameController()
      setState(controller, baseState(Phase.EndEval).copy(score = 1000))

      controller.start()
      controller.state.phase shouldBe Phase.Win
    }

    "move EndEval phase to Lose" in {
      val controller = new GameController()
      setState(controller, baseState(Phase.EndEval).copy(plays = 0, score = 0))

      controller.start()
      controller.state.phase shouldBe Phase.Lose
    }

    "reject command in automatic phases" in {
      val controller = new GameController()
      setState(controller, baseState(Phase.Draw))

      controller.handle(GameCommand.Help).isLeft shouldBe true
    }

    // Tests for GameController methods (migrated from ModelTest.scala)
    "select dice from available dice" in {
      val controller = new GameController()
      val initialState = baseState(Phase.Select).copy(
        availableDice = List(plain, plain, plain)
      )
      setState(controller, initialState)
      
      val state = controller.selectDice(initialState, List(0, 1))
      state.availableDice.length shouldBe 1
      state.selectedDice.length shouldBe 2
    }

    "ignore invalid selection indices" in {
      val controller = new GameController()
      val initialState = baseState(Phase.Select).copy(
        availableDice = List(plain, plain, plain)
      )
      setState(controller, initialState)
      
      controller.selectDice(initialState, List(999)) shouldBe initialState
    }

    "move selected dice to play" in {
      val controller = new GameController()
      val withSelected = baseState(Phase.Select).copy(
        selectedDice = List(plain, plain)
      )
      setState(controller, withSelected)
      
      val played = controller.addDiceToPlay(withSelected)

      played.selectedDice shouldBe Nil
      played.diceInPlay.length shouldBe 2
    }

    "not move dice to play if no dice are selected" in {
      val controller = new GameController()
      val initialState = baseState(Phase.Select)
      setState(controller, initialState)
      
      controller.addDiceToPlay(initialState) shouldBe initialState
    }

    "discard selected dice and reduce discards" in {
      val controller = new GameController()
      val withSelected = baseState(Phase.Select).copy(
        selectedDice = List(plain),
        discards = 4
      )
      setState(controller, withSelected)
      
      val selected = controller.selectDice(withSelected, List(0))
      val discarded = controller.discardDice(withSelected)

      discarded.discards shouldBe 3
      discarded.selectedDice shouldBe Nil
    }

    "not discard if nothing selected or no discards left" in {
      val controller = new GameController()
      val state1 = baseState(Phase.Select)
      val state2 = baseState(Phase.Select).copy(selectedDice = List(plain), discards = 0)
      setState(controller, state1)
      
      controller.discardDice(state1) shouldBe state1
      controller.discardDice(state2) shouldBe state2
    }

    "select played dice for reroll" in {
      val controller = new GameController()
      val state = baseState(Phase.PickOut).copy(
        diceInPlay = List(
          RolledDie(plain, 1),
          RolledDie(plain, 2)
        )
      )
      setState(controller, state)

      val selected = controller.selectPlayedDice(state, List(0))

      selected.diceInPlay.length shouldBe 1
      selected.diceToRoll.length shouldBe 1
    }

    "ignore invalid played dice indices" in {
      val controller = new GameController()
      val state = baseState(Phase.PickOut)
      setState(controller, state)
      
      controller.selectPlayedDice(state, List(999)) shouldBe state
    }

    "reroll selected dice" in {
      val controller = new GameController()
      val state = baseState(Phase.Roll).copy(
        diceToRoll = List(RolledDie(plain, 1)),
        rerolls = 2
      )
      setState(controller, state)

      val rolled = controller.rollDice(state)

      rolled.diceToRoll shouldBe Nil
      rolled.diceInPlay.length shouldBe 1
      rolled.rerolls shouldBe 1
    }

    "not reroll without rerolls or dice to roll" in {
      val controller = new GameController()
      val noRerolls = baseState(Phase.Roll).copy(
        rerolls = 0,
        diceToRoll = List(RolledDie(plain, 1))
      )
      val noDiceToRoll = baseState(Phase.Roll).copy(
        rerolls = 2,
        diceToRoll = Nil
      )
      setState(controller, noRerolls)

      controller.rollDice(noRerolls) shouldBe noRerolls
      controller.rollDice(noDiceToRoll) shouldBe noDiceToRoll
    }

    "score dice in play and create locked row" in {
      val controller = new GameController()
      val state = baseState(Phase.Score).copy(
        diceInPlay = List(
          RolledDie(plain, 6),
          RolledDie(plain, 6),
          RolledDie(plain, 1)
        )
      )
      setState(controller, state)

      val scored = controller.scoreDiceInPlay(state)

      scored.score should be > 0
      scored.lockedRows.length shouldBe 1
      scored.diceInPlay shouldBe Nil
    }

    "score dice with cupgrade effect" in {
      val controller = new GameController()
      val state = baseState(Phase.Score).copy(
        diceInPlay = List(
          RolledDie(plain, 6),
          RolledDie(plain, 6)
        ),
        cupgrades = List(Cupgrade("bonus", s => s.copy(score = s.score + 10)))
      )
      setState(controller, state)

      val scored = controller.scoreDiceInPlay(state)

      scored.score should be > 10
    }

    "not score empty dice list" in {
      val controller = new GameController()
      val state = baseState(Phase.Score)
      setState(controller, state)
      
      controller.scoreDiceInPlay(state) shouldBe state
    }
  }
}
