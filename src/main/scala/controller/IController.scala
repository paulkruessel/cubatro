package controller

import model.*
import util.Observer

trait IController:
  def state: GameState
  def viewState: GameViewState

  def isRunning: Boolean
  def isRunning_=(value: Boolean): Unit

  def start(): Unit
  def handle(command: GameCommand): Either[String, GameState]

  def drawDice(oldState: GameState): GameState
  def selectDice(oldState: GameState, indices: List[Int]): GameState
  def discardDice(oldState: GameState): GameState
  def addDiceToPlay(oldState: GameState): GameState
  def selectPlayedDice(oldState: GameState, indices: List[Int]): GameState
  def rollDice(oldState: GameState): GameState
  def scoreDiceInPlay(oldState: GameState): GameState

  def add(observer: Observer): Unit
  def remove(observer: Observer): Unit

enum GameCommand:
  case Help
  case Quit
  case Select(indices: List[Int])
  case Discard
  case PlaySelected
  case Pick(indices: List[Int])
  case Reroll
  case ScoreCurrent
  case Undo
  case Redo
  case Invalid

case class GameViewState(
    targetScore: Int,
    score: Int,
    phase: String,
    plays: Int,
    rerolls: Int,
    discards: Int,
    hand: List[String],
    selected: List[String],
    inPlay: List[String],
    toRoll: List[String],
    lockedRows: List[String],
    isWin: Boolean,
    isLose: Boolean,
    handDice: List[DieView] = Nil,
    selectedDiceViews: List[DieView] = Nil,
    inPlayDice: List[DieView] = Nil,
    toRollDice: List[DieView] = Nil
)

trait DieView(
    text: String,
    bonusType: BonusType,
    bonusValue: Int,
    guiText: String
):
  def tooltip: String =
    bonusType match
      case BonusType.None  => "Bonus: none"
      case BonusType.Chips => s"Bonus: +${bonusValue} Chips"
      case BonusType.Mult  => s"Bonus: +${bonusValue} Mult"