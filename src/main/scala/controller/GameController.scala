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

  def handle(command: GameCommand): Either[String, GameState] =
    val result =
      currentState.phase match
        case Phase.Select =>
          command match
            case GameCommand.Select(indices) =>
              Right(currentState.selectDice(indices))
            case GameCommand.Discard =>
              Right(currentState.discardDice())
            case GameCommand.PlaySelected =>
              Right(currentState.addDiceToPlay().copy(phase = Phase.Roll))
            case _ =>
              Left("Allowed: select, discard, play, help, quit")

        case Phase.PickOut =>
          command match
            case GameCommand.Pick(indices) =>
              Right(currentState.selectPlayedDice(indices))
            case GameCommand.Reroll =>
              Right(currentState.copy(phase = Phase.Roll))
            case GameCommand.ScoreCurrent =>
              val scored = currentState.scoreDiceInPlay()
              Right(scored.copy(plays = math.max(0, scored.plays - 1), phase = Phase.EndEval))
            case _ =>
              Left("Allowed: pick, reroll, score, help, quit")

        case Phase.Score =>
          command match
            case GameCommand.ScoreCurrent =>
              val scored = currentState.scoreDiceInPlay()
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

  private def advance(initial: GameState): GameState =
    var s = initial
    var continue = true

    while continue do
      s.phase match
        case Phase.Draw =>
          s = s.drawDice().copy(phase = Phase.Select)

        case Phase.Roll =>
          val rolled = s.rollDice()
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
