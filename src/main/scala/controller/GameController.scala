package controller

import model.*
import util.Observable
import scala.util.Random

enum GameCommand:
  case Help
  case Quit
  case Select(indices: List[Int])
  case Discard
  case PlaySelected
  case Pick(indices: List[Int])
  case Reroll
  case ScoreCurrent

class GameController extends Observable:
  private var currentState: GameState = GameController.defaultInitialState()

  def state: GameState = currentState

  def drawDice(oldState: GameState): GameState =
    val drawCount = math.max(0, math.min(oldState.maxAvailableDice - oldState.availableDice.length, oldState.bag.size))
    val shuffled = Random.shuffle(oldState.bag)
    oldState.copy(
      bag = shuffled.drop(drawCount),
      availableDice = oldState.availableDice ++ shuffled.take(drawCount)
    )

  def selectDice(oldState: GameState, indices: List[Int]): GameState =
    val valid = indices.filter(i => i >= 0 && i < oldState.availableDice.length).distinct
    val selected = valid.take(math.max(0, 5 - oldState.selectedDice.length))
    if selected.isEmpty then oldState
    else
      val dice = selected.map(oldState.availableDice)
      val remaining = selected.sorted.reverse.foldLeft(oldState.availableDice)(removeAt)
      oldState.copy(availableDice = remaining, selectedDice = oldState.selectedDice ++ dice)

  def discardDice(oldState: GameState): GameState =
    if oldState.discards <= 0 || oldState.selectedDice.isEmpty then oldState
    else drawDice(oldState.copy(selectedDice = Nil, discards = oldState.discards - 1))

  def addDiceToPlay(oldState: GameState): GameState =
    if oldState.selectedDice.isEmpty then oldState
    else
      val rolled = oldState.selectedDice.map(_.roll())
      drawDice(
        oldState.copy(
          diceInPlay = (oldState.diceInPlay ++ rolled).sortBy(_.value),
          selectedDice = Nil
        )
      )

  def selectPlayedDice(oldState: GameState, indices: List[Int]): GameState =
    val valid = indices.filter(i => i >= 0 && i < oldState.diceInPlay.length).distinct
    if valid.isEmpty then oldState
    else
      val moved = valid.map(oldState.diceInPlay)
      val remaining = valid.sorted.reverse.foldLeft(oldState.diceInPlay)(removeAt)
      oldState.copy(diceInPlay = remaining, diceToRoll = oldState.diceToRoll ++ moved)

  def rollDice(oldState: GameState): GameState =
    if oldState.rerolls <= 0 || oldState.diceToRoll.isEmpty then oldState
    else
      val rolled = oldState.diceToRoll.map(_.die.roll())
      oldState.copy(
        diceInPlay = (oldState.diceInPlay ++ rolled).sortBy(_.value),
        diceToRoll = Nil,
        rerolls = oldState.rerolls - 1
      )

  def scoreDiceInPlay(oldState: GameState): GameState =
    if oldState.diceInPlay.isEmpty then oldState
    else
      val diceChips = oldState.diceInPlay.map(_.eval()._1).sum
      val diceMult = oldState.diceInPlay.map(_.eval()._2).sum
      val combination = matchingCombinations(oldState.diceInPlay).last
      val combinations = matchingCombinations(oldState.diceInPlay)
      val extraScore = (diceChips + combination.chips) * (diceMult + combination.mult)
      val scored = oldState.copy(score = oldState.score + extraScore)
      val upgraded = oldState.cupgrades.foldLeft(scored)((s, c) => c.effect(s))
      val row = LockedRow(oldState.diceInPlay, combination, extraScore)

      upgraded.copy(
        diceInPlay = Nil,
        diceToRoll = Nil,
        lockedRows = upgraded.lockedRows :+ row,
        rerolls = oldState.totalRerolls
      )

  def handle(command: GameCommand): Either[String, GameState] =
    val result =
      currentState.phase match
        case Phase.Select =>
          command match
            case GameCommand.Select(indices) =>
              Right(selectDice(currentState, indices))
            case GameCommand.Discard =>
              Right(discardDice(currentState))
            case GameCommand.PlaySelected =>
              Right(addDiceToPlay(currentState).copy(phase = Phase.Roll))
            case _ =>
              Left("Allowed: select, discard, play, help, quit")

        case Phase.PickOut =>
          command match
            case GameCommand.Pick(indices) =>
              Right(selectPlayedDice(currentState, indices))
            case GameCommand.Reroll =>
              Right(currentState.copy(phase = Phase.Roll))
            case GameCommand.ScoreCurrent =>
              val scored = scoreDiceInPlay(currentState)
              Right(scored.copy(plays = math.max(0, scored.plays - 1), phase = Phase.EndEval))
            case _ =>
              Left("Allowed: pick, reroll, score, help, quit")

        case Phase.Score =>
          command match
            case GameCommand.ScoreCurrent =>
              val scored = scoreDiceInPlay(currentState)
              Right(scored.copy(plays = math.max(0, scored.plays - 1), phase = Phase.EndEval))
            case _ =>
              Left("Allowed: score, help, quit")

        case _ =>
          Left(s"No command allowed in phase ${currentState.phase}")

    result match
      case Right(next) =>
        currentState = advance(next)
        notifyObservers()
        Right(currentState)
      case Left(error) =>
        Left(error)

  def start(): Unit =
    currentState = advance(currentState)
    notifyObservers()

  private def advance(state: GameState): GameState =
    var s = state
    var continue = true

    while continue do
      s.phase match
        case Phase.Draw =>
          s = drawDice(s).copy(phase = Phase.Select)

        case Phase.Roll =>
          val rolled = rollDice(s)
          s =
            if rolled.rerolls <= 0 then rolled.copy(phase = Phase.Score)
            else rolled.copy(phase = Phase.PickOut)

        case Phase.EndEval =>
          s =
            if s.score >= s.targetScore then s.copy(phase = Phase.Win)
            else if s.plays <= 0 then s.copy(phase = Phase.Lose)
            else s.copy(phase = Phase.Draw)

        case _ =>
          continue = false

    s

object GameController:
  def defaultInitialState(): GameState =
    val plain = Die(bonusType = BonusType.None, bonusValue = 0)
    val chips = Die(bonusType = BonusType.Chips, bonusValue = 2)
    val mult = Die(bonusType = BonusType.Mult, bonusValue = 1)

    GameState(
      bag = Random.shuffle(List.fill(14)(plain) ++ List.fill(6)(chips) ++ List.fill(4)(mult)),
      availableDice = Nil,
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
      phase = Phase.Draw
    )
