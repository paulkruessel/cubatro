import controller.internal_controller.*
import model.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PhaseStateTest extends AnyWordSpec with Matchers:

  private val plainDie = Die(1, 6, BonusType.None, 0)

  private def state(
      phase: Phase,
      score: Int = 0,
      targetScore: Int = 1000,
      plays: Int = 6,
      rerolls: Int = 4
  ): GameState =
    GameState(
      bag = List(plainDie),
      availableDice = Nil,
      maxAvailableDice = 8,
      selectedDice = Nil,
      diceInPlay = Nil,
      diceToRoll = Nil,
      lockedRows = Nil,
      cupgrades = Nil,
      discards = 4,
      rerolls = rerolls,
      totalRerolls = 4,
      plays = plays,
      targetScore = targetScore,
      score = score,
      phase = phase
    )

  private val unchangedDraw: GameState => GameState = identity
  private val unchangedRoll: GameState => GameState = identity

  "PhaseState" should {

    "resolve automatic and waiting phase states" in {
      PhaseState.forPhase(Phase.Draw) shouldBe DrawPhaseState
      PhaseState.forPhase(Phase.Roll) shouldBe RollPhaseState
      PhaseState.forPhase(Phase.EndEval) shouldBe EndEvalPhaseState
      PhaseState.forPhase(Phase.Select) shouldBe WaitingPhaseState
      PhaseState.forPhase(Phase.Win).isAutomatic shouldBe false
    }

    "advance Draw by drawing dice and entering Select" in {
      var drawWasUsed = false
      val initial = state(Phase.Draw)
      val advanced = DrawPhaseState.advance(
        initial,
        oldState => {
          drawWasUsed = true
          oldState.copy(availableDice = List(plainDie))
        },
        unchangedRoll
      )

      drawWasUsed shouldBe true
      advanced.phase shouldBe Phase.Select
      advanced.availableDice shouldBe List(plainDie)
    }

    "advance Roll to PickOut when rerolls remain" in {
      val advanced = RollPhaseState.advance(
        state(Phase.Roll),
        unchangedDraw,
        oldState => oldState.copy(rerolls = 2)
      )

      advanced.phase shouldBe Phase.PickOut
      advanced.rerolls shouldBe 2
    }

    "advance Roll to Score when no rerolls remain" in {
      val advanced = RollPhaseState.advance(
        state(Phase.Roll),
        unchangedDraw,
        oldState => oldState.copy(rerolls = 0)
      )

      advanced.phase shouldBe Phase.Score
      advanced.rerolls shouldBe 0
    }

    "advance EndEval to Win, Lose or Draw" in {
      EndEvalPhaseState
        .advance(state(Phase.EndEval, score = 1000, targetScore = 1000), unchangedDraw, unchangedRoll)
        .phase shouldBe Phase.Win

      EndEvalPhaseState
        .advance(state(Phase.EndEval, plays = 0), unchangedDraw, unchangedRoll)
        .phase shouldBe Phase.Lose

      EndEvalPhaseState
        .advance(state(Phase.EndEval, score = 5, targetScore = 1000, plays = 1), unchangedDraw, unchangedRoll)
        .phase shouldBe Phase.Draw
    }

    "leave waiting phases unchanged" in {
      val initial = state(Phase.Select)

      WaitingPhaseState.isAutomatic shouldBe false
      WaitingPhaseState.advance(initial, unchangedDraw, unchangedRoll) shouldBe theSameInstanceAs(initial)
    }
  }
