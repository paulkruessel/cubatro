package controller

import com.google.inject.Inject
import model.*
import fileio.FileIO
import util.Observable
import scala.util.{Failure, Random, Success, Try}

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
  case Save(path: String)
  case Load(path: String)
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

case class DieView(
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

object DieView:
  def apply(text: String, bonusType: BonusType, bonusValue: Int): DieView =
    new DieView(text, bonusType, bonusValue, text.dropWhile(_.isDigit).stripPrefix(":"))

class GameController(
    initialState: GameState = GameController.defaultInitialState(),
    undoManager: UndoManager = new UndoManager(),
    fileIO: FileIO = new fileio.JsonFileIO()
) extends Observable with IController:

  @Inject
  def this(fileIO: FileIO) =
    this(GameController.defaultInitialState(), new UndoManager(), fileIO)

  private var currentState: GameState = initialState

  var isRunning: Boolean = false

  def state: GameState = currentState

  def defaultSavePath: String =
    fileIO.defaultSavePath

  def save(path: String): Unit =
    fileIO.save(currentState, path)

  def load(path: String): GameState =
    currentState = fileIO.load(path)
    notifyObservers()
    currentState

  def viewState: GameViewState =
    val handDice = currentState.availableDice.zipWithIndex.map((die, index) => dieView(index, die))
    val selectedDice = currentState.selectedDice.zipWithIndex.map((die, index) => dieView(index, die))
    val inPlayDice = currentState.diceInPlay.zipWithIndex.map((die, index) => rolledDieView(index, die))
    val toRollDice = currentState.diceToRoll.zipWithIndex.map((die, index) => rolledDieView(index, die))

    GameViewState(
      targetScore = currentState.targetScore,
      score = currentState.score,
      phase = currentState.phase.toString,
      plays = currentState.plays,
      rerolls = currentState.rerolls,
      discards = currentState.discards,
      hand = handDice.map(_.text),
      selected = selectedDice.map(_.text),
      inPlay = inPlayDice.map(_.text),
      toRoll = toRollDice.map(_.text),
      lockedRows = currentState.lockedRows.zipWithIndex.map((r, i) => s"${i + 1}. ${r.combination} -> ${r.score}"),
      isWin = currentState.phase == Phase.Win,
      isLose = currentState.phase == Phase.Lose,
      handDice = handDice,
      selectedDiceViews = selectedDice,
      inPlayDice = inPlayDice,
      toRollDice = toRollDice
    )

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
      oldState.copy(
        availableDice = remaining,
        selectedDice = oldState.selectedDice ++ dice
      )

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
      oldState.copy(
        diceInPlay = remaining,
        diceToRoll = oldState.diceToRoll ++ moved
      )

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
      val combination = matchingCombinations(oldState.diceInPlay).last
      val diceChips = oldState.diceInPlay.map(_.eval()._1).sum
      val diceMult = oldState.diceInPlay.map(_.eval()._2).sum
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
    command match
      case GameCommand.Quit =>
        isRunning = false
        notifyObservers()
        Left("Game ended by player.")
      case GameCommand.Save(path) =>
        Try(save(path)) match
          case Success(_)     => Right(currentState)
          case Failure(error) => Left(s"Could not save game: ${error.getMessage}")

      case GameCommand.Load(path) =>
        Try(load(path)) match
          case Success(loadedState) => Right(loadedState)
          case Failure(error)       => Left(s"Could not load game: ${error.getMessage}")

      case GameCommand.Undo =>
        undoManager.undoStep() match
          case Some(_) =>
            notifyObservers()
            Right(currentState)
          case None =>
            Left("Nothing to undo")

      case GameCommand.Redo =>
        undoManager.redoStep() match
          case Some(_) =>
            notifyObservers()
            Right(currentState)
          case None =>
            Left("Nothing to redo")

      case _ =>
        val beforeState = currentState
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
                case GameCommand.Invalid => Left("Unknown command. Use help to see valid commands.")
                case _ =>
                  Left("Allowed: select, discard, play, save, load, help, quit, undo, redo")

            case Phase.PickOut =>
              command match
                case GameCommand.Pick(indices) =>
                  Right(selectPlayedDice(currentState, indices))
                case GameCommand.Reroll =>
                  Right(currentState.copy(phase = Phase.Roll))
                case GameCommand.ScoreCurrent =>
                  val scored = scoreDiceInPlay(currentState)
                  Right(scored.copy(plays = math.max(0, scored.plays - 1), phase = Phase.EndEval))
                case GameCommand.Invalid => Left("Unknown command. Use help to see valid commands.")
                case _ =>
                  Left("Allowed: pick, reroll, score, save, load, help, quit, undo, redo")

            case Phase.Score =>
              command match
                case GameCommand.ScoreCurrent =>
                  val scored = scoreDiceInPlay(currentState)
                  Right(scored.copy(plays = math.max(0, scored.plays - 1), phase = Phase.EndEval))
                case GameCommand.Invalid => Left("Unknown command. Use help to see valid commands.")
                case _ =>
                  Left("Allowed: score, save, load, help, quit, undo, redo")

            case _ =>
              Left(s"No command allowed in phase ${currentState.phase}")

        result match
          case Right(next) =>
            val afterState = advance(next)
            if afterState != beforeState then
              val commandToStore = GameStateCommand(beforeState, afterState, state => currentState = state)
              undoManager.doStep(commandToStore)
            else
              currentState = afterState
            notifyObservers()
            Right(currentState)
          case Left(error) =>
            Left(error)

  def start(): Unit =
    isRunning = true
    currentState = advance(currentState)
    notifyObservers()

  private def advance(state: GameState): GameState =
    var s = state
    var continue = true

    while continue do
      val phaseState = PhaseState.forPhase(s.phase)
      if phaseState.isAutomatic then
        s = phaseState.advance(s, drawDice, rollDice)
      else
        continue = false

    s

  private def dieView(index: Int, die: Die): DieView =
    val text = dieText(die)
    DieView(s"$index:$text", die.bonusType, die.bonusValue, text)

  private def rolledDieView(index: Int, die: RolledDie): DieView =
    DieView(s"$index:${rolledDieText(die)}", die.die.bonusType, die.die.bonusValue, s"[${die.value}]")

  private def dieText(die: Die): String =
    die.bonusType match
      case BonusType.None  => s"[d${die.min}-${die.max}]"
      case BonusType.Chips => s"[d${die.min}-${die.max}:+${die.bonusValue}C]"
      case BonusType.Mult  => s"[d${die.min}-${die.max}:+${die.bonusValue}M]"

  private def rolledDieText(die: RolledDie): String =
    die.die.bonusType match
      case BonusType.None  => s"[${die.value}]"
      case BonusType.Chips => s"[${die.value}:+${die.die.bonusValue}C]"
      case BonusType.Mult  => s"[${die.value}:+${die.die.bonusValue}M]"

object GameController:
  def defaultInitialState(): GameState =
    val plain = DieFactory.plain
    val chips = DieFactory.chips(2)
    val mult = DieFactory.mult(2)

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
      targetScore = 5000,
      score = 0,
      phase = Phase.Draw
    )
