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
  }
}
