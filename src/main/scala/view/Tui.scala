package view

import com.google.inject.Inject
import controller.*
import util.Observer
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

class Tui(
    controller: IController,
    readInput: () => String = () => StdIn.readLine(),
    writeOutput: String => Unit = text => print(text)
) extends IView:

  @Inject
  def this(controller: IController) =
    this(controller, () => StdIn.readLine(), text => print(text))

  controller.add(this)

  override def update(): Unit =
    writeOutput(render(controller.viewState) + "\n")

  def run(): Unit =
    if !controller.isRunning then
      controller.start()

    while controller.isRunning do
      writeOutput(prompt(controller.viewState.phase))
      val input = readInput()

      parseSafe(input) match
        case Failure(error) =>
          writeOutput(s"Action error: ${error.getMessage}\n")

        case Success(GameCommand.Help) =>
          writeOutput(help(controller.viewState.phase) + "\n")

        case Success(GameCommand.Quit) =>
          controller.handle(GameCommand.Quit)
          writeOutput("Game stopped by player.\n")

        case Success(command) =>
          controller.handle(command) match
            case Left(error) =>
              writeOutput(s"Action error: $error\n")
            case Right(_) =>
              successMessage(command).foreach(message => writeOutput(message + "\n"))
              val viewState = controller.viewState
              if viewState.isWin || viewState.isLose then
                writeOutput(if viewState.isWin then "You win.\n" else "You lose.\n")
                controller.isRunning = false

  def parseSafe(input: String): Try[GameCommand] =
    Try {
      val normalized = Option(input).map(_.trim).getOrElse("")
      if normalized.isEmpty then
        throw new IllegalArgumentException("No command entered. Use help to see valid commands.")

      parse(normalized)
    }

  def parse(input: String): GameCommand =
    val trimmed = input.trim
    val command = trimmed.takeWhile(!_.isWhitespace).toLowerCase
    val arguments = trimmed.drop(command.length).trim

    command match
      case "help" | "h" if arguments.isEmpty    => GameCommand.Help
      case "quit" | "q" if arguments.isEmpty    => GameCommand.Quit
      case "discard" | "d" if arguments.isEmpty => GameCommand.Discard
      case "play" | "p" if arguments.isEmpty    => GameCommand.PlaySelected
      case "reroll" | "r" if arguments.isEmpty  => GameCommand.Reroll
      case "score" | "s" if arguments.isEmpty   => GameCommand.ScoreCurrent
      case "undo" | "u" if arguments.isEmpty    => GameCommand.Undo
      case "redo" if arguments.isEmpty          => GameCommand.Redo
      case "save"                               => GameCommand.Save(pathOrDefault(arguments))
      case "load" | "l"                         => GameCommand.Load(pathOrDefault(arguments))
      case "select"                             => GameCommand.Select(parseIndices(indexTokens(arguments)))
      case "pick"                               => GameCommand.Pick(parseIndices(indexTokens(arguments)))
      case _                                    => GameCommand.Invalid

  private def parseIndices(tokens: List[String]): List[Int] =
    if tokens.isEmpty then
      throw new IllegalArgumentException("No indices entered. Use help to see valid commands.")

    tokens.map { token =>
      Try(token.toInt) match
        case Success(index) => index
        case Failure(_) =>
          throw new IllegalArgumentException(s"'$token' is not a valid index. Use whole numbers only.")
    }

  private def indexTokens(arguments: String): List[String] =
    arguments.replace(",", " ").split("\\s+").toList.filter(_.nonEmpty)

  private def pathOrDefault(arguments: String): String =
    if arguments.isEmpty then controller.defaultSavePath else arguments

  private def successMessage(command: GameCommand): Option[String] =
    command match
      case GameCommand.Save(path) => Some(s"Game saved to $path.")
      case GameCommand.Load(path) => Some(s"Game loaded from $path.")
      case _                      => None

  def render(viewState: GameViewState): String =
    val hand =
      if viewState.hand.isEmpty then "-"
      else viewState.hand.mkString(" ")

    val selected =
      if viewState.selected.isEmpty then "-"
      else viewState.selected.mkString(" ")

    val inPlay =
      if viewState.inPlay.isEmpty then "-"
      else viewState.inPlay.mkString(" ")

    val toRoll =
      if viewState.toRoll.isEmpty then "-"
      else viewState.toRoll.mkString(" ")

    val rows =
      if viewState.lockedRows.isEmpty then "-"
      else viewState.lockedRows.mkString("\n")

    s"""
+----------------------------------------------------------------------+
|                               CUBATRO                                |
+----------------------------------------------------------------------+
Target: ${viewState.targetScore} | Score: ${viewState.score} | Phase: ${viewState.phase}
Plays: ${viewState.plays} | Rerolls: ${viewState.rerolls} | Discards: ${viewState.discards}

Hand:
$hand

Selected:
$selected

In Play:
$inPlay

To Roll:
$toRoll

Locked rows:
$rows
+----------------------------------------------------------------------+
""".trim

  def prompt(phase: String): String =
    phase match
      case "Select"  => "\nSelect phase: select <indices> | discard | play | save [path] | load [path] | undo | redo | help | quit\n> "
      case "PickOut" => "\nPickOut phase: pick <indices> | reroll | score | save [path] | load [path] | undo | redo | help | quit\n> "
      case "Score"   => "\nScore phase: score | save [path] | load [path] | undo | redo | help | quit\n> "
      case _         => s"\nPhase $phase: save [path] | load [path] | undo | redo | help | quit\n> "

  def help(phase: String): String =
    phase match
      case "Select"  => "Select dice with: select 0 1 2. Then use: play. Save with: save [path]. Load with: load [path]. Undo with: undo. Redo with: redo."
      case "PickOut" => "Use: pick 0, then reroll. Or use: score. Save with: save [path]. Load with: load [path]. Undo with: undo. Redo with: redo."
      case "Score"   => "Use: score. Save with: save [path]. Load with: load [path]. Undo with: undo. Redo with: redo."
      case _         => "Commands: save [path], load [path], undo, redo, help, quit."
