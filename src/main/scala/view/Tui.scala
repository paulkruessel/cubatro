package view

import controller.*
import util.Observer
import scala.io.StdIn
import scala.util.Try

class Tui(
    controller: IController,
    readInput: () => String = () => StdIn.readLine(),
    writeOutput: String => Unit = text => print(text)
) extends IView:

  controller.add(this)

  override def update(): Unit =
    writeOutput(render(controller.viewState) + "\n")

  def run(): Unit =
    if !controller.isRunning then
      controller.start()

    while controller.isRunning do
      writeOutput(prompt(controller.viewState.phase))
      val input = readInput()

      parseSafe(input).getOrElse(GameCommand.Invalid) match
        case GameCommand.Help =>
          writeOutput(help(controller.viewState.phase) + "\n")

        case GameCommand.Quit =>
          controller.handle(GameCommand.Quit)
          writeOutput("Game stopped by player.\n")

        case command =>
          controller.handle(command) match
            case Left(error) =>
              writeOutput(s"Action error: $error\n")
            case Right(_) =>
              val viewState = controller.viewState
              if viewState.isWin || viewState.isLose then
                writeOutput(if viewState.isWin then "You win.\n" else "You lose.\n")
                controller.isRunning = false

  def parseSafe(input: String): Try[GameCommand] =
    Try(parse(input))

  def parse(input: String): GameCommand =
    val tokens = input.trim.toLowerCase.replace(",", " ").split("\\s+").toList.filter(_.nonEmpty)

    tokens match
      case "help" :: Nil | "h" :: Nil       => GameCommand.Help
      case "quit" :: Nil | "q" :: Nil       => GameCommand.Quit
      case "discard" :: Nil | "d" :: Nil    => GameCommand.Discard
      case "play" :: Nil | "p" :: Nil       => GameCommand.PlaySelected
      case "reroll" :: Nil | "r" :: Nil     => GameCommand.Reroll
      case "score" :: Nil | "s" :: Nil      => GameCommand.ScoreCurrent
      case "undo" :: Nil | "u" :: Nil       => GameCommand.Undo
      case "redo" :: Nil                     => GameCommand.Redo
      case "select" :: tail                 => GameCommand.Select(tail.flatMap(_.toIntOption))
      case "pick" :: tail                   => GameCommand.Pick(tail.flatMap(_.toIntOption))
      case _                                => GameCommand.Invalid

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
      case "Select"  => "\nSelect phase: select <indices> | discard | play | undo | redo | help | quit\n> "
      case "PickOut" => "\nPickOut phase: pick <indices> | reroll | score | undo | redo | help | quit\n> "
      case "Score"   => "\nScore phase: score | undo | redo | help | quit\n> "
      case _         => s"\nPhase $phase\n> "

  def help(phase: String): String =
    phase match
      case "Select"  => "Select dice with: select 0 1 2. Then use: play. Undo with: undo. Redo with: redo."
      case "PickOut" => "Use: pick 0, then reroll. Or use: score. Undo with: undo. Redo with: redo."
      case "Score"   => "Use: score. Undo with: undo. Redo with: redo."
      case _         => "Commands: undo, redo, help, quit."