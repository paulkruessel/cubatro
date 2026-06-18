package controller

import model.*

trait PhaseState:
  def isAutomatic: Boolean
  def advance(
      state: GameState,
      drawDice: GameState => GameState,
      rollDice: GameState => GameState
  ): GameState

object PhaseState:
  def forPhase(phase: Phase): PhaseState =
    phase match
      case Phase.Draw    => DrawPhaseState
      case Phase.Roll    => RollPhaseState
      case Phase.EndEval => EndEvalPhaseState
      case _             => WaitingPhaseState

object DrawPhaseState extends PhaseState:
  override val isAutomatic: Boolean = true

  override def advance(
      state: GameState,
      drawDice: GameState => GameState,
      rollDice: GameState => GameState
  ): GameState =
    drawDice(state).copy(phase = Phase.Select)

object RollPhaseState extends PhaseState:
  override val isAutomatic: Boolean = true

  override def advance(
      state: GameState,
      drawDice: GameState => GameState,
      rollDice: GameState => GameState
  ): GameState =
    val rolled = rollDice(state)
    if rolled.rerolls <= 0 then rolled.copy(phase = Phase.Score)
    else rolled.copy(phase = Phase.PickOut)

object EndEvalPhaseState extends PhaseState:
  override val isAutomatic: Boolean = true

  override def advance(
      state: GameState,
      drawDice: GameState => GameState,
      rollDice: GameState => GameState
  ): GameState =
    if state.score >= state.targetScore then state.copy(phase = Phase.Win)
    else if state.plays <= 0 then state.copy(phase = Phase.Lose)
    else state.copy(phase = Phase.Draw)

object WaitingPhaseState extends PhaseState:
  override val isAutomatic: Boolean = false

  override def advance(
      state: GameState,
      drawDice: GameState => GameState,
      rollDice: GameState => GameState
  ): GameState =
    state
