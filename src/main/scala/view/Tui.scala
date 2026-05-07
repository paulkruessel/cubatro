package view

import controller.*
import model.*
import util.Observer
import scala.io.StdIn

class Tui(
    controller: GameController,
    readInput: () => String = () => StdIn.readLine(),
    writeOutput: String => Unit = text => print(text)
) extends Observer:

  controller.add(this)

  override def update(): Unit =
    writeOutput(render(controller.state) + "\n")

  def run(): Unit =
    controller.start()
    var running = true

    while running do
      writeOutput(prompt(controller.state.phase))
      val input = readInput()

      parse(input) match
        case GameCommand.Quit =>
          writeOutput("Game stopped by player.\n")
          running = false

        case GameCommand.Help =>
          writeOutput(help(controller.state.phase) + "\n")

        case command =>
          controller.handle(command) match
            case Left(error) =>
              writeOutput(s"Action error: $error\n")
            case Right(state) =>
              if state.phase == Phase.Win || state.phase == Phase.Lose then
                writeOutput(if state.phase == Phase.Win then "You win.\n" else "You lose.\n")
                running = false

  def parse(input: String): GameCommand =
    val tokens = input.trim.toLowerCase.replace(",", " ").split("\\s+").toList.filter(_.nonEmpty)

    tokens match
      case "help" :: Nil | "h" :: Nil       => GameCommand.Help
      case "quit" :: Nil | "q" :: Nil       => GameCommand.Quit
      case "discard" :: Nil | "d" :: Nil    => GameCommand.Discard
      case "play" :: Nil | "p" :: Nil       => GameCommand.PlaySelected
      case "reroll" :: Nil | "r" :: Nil     => GameCommand.Reroll
      case "score" :: Nil | "s" :: Nil      => GameCommand.ScoreCurrent
      case "select" :: tail                 => GameCommand.Select(tail.flatMap(_.toIntOption))
      case "pick" :: tail                   => GameCommand.Pick(tail.flatMap(_.toIntOption))
      case _                                => GameCommand.Help

  def render(state: GameState): String =
    val hand =
      if state.availableDice.isEmpty then "-"
      else state.availableDice.zipWithIndex.map((d, i) => s"$i:${dieText(d)}").mkString(" ")

    val selected =
      if state.selectedDice.isEmpty then "-"
      else state.selectedDice.zipWithIndex.map((d, i) => s"$i:${dieText(d)}").mkString(" ")

    val inPlay =
      if state.diceInPlay.isEmpty then "-"
      else state.diceInPlay.zipWithIndex.map((d, i) => s"$i:[${d.value}]").mkString(" ")

    val rows =
      if state.lockedRows.isEmpty then "-"
      else state.lockedRows.zipWithIndex.map((r, i) => s"${i + 1}. ${r.combination} -> ${r.score}").mkString("\n")

    s"""
+----------------------------------------------------------------------+
|                               CUBATRO                                |
+----------------------------------------------------------------------+
Target: ${state.targetScore} | Score: ${state.score} | Phase: ${state.phase}
Plays: ${state.plays} | Rerolls: ${state.rerolls} | Discards: ${state.discards}

Hand:
$hand

Selected:
$selected

In Play:
$inPlay

Locked rows:
$rows
+----------------------------------------------------------------------+
""".trim

  def prompt(phase: Phase): String =
    phase match
      case Phase.Select  => "\nSelect phase: select <indices> | discard | play | help | quit\n> "
      case Phase.PickOut => "\nPickOut phase: pick <indices> | reroll | score | help | quit\n> "
      case Phase.Score   => "\nScore phase: score | help | quit\n> "
      case _             => s"\nPhase $phase\n> "

  def help(phase: Phase): String =
    phase match
      case Phase.Select  => "Select dice with: select 0 1 2. Then use: play."
      case Phase.PickOut => "Use: pick 0, then reroll. Or use: score."
      case Phase.Score   => "Use: score."
      case _             => "Commands: help, quit."

  private def dieText(die: Die): String =
    die.bonusType match
      case BonusType.None  => s"[d${die.min}-${die.max}]"
      case BonusType.Chips => s"[d${die.min}-${die.max}:+${die.bonusValue}C]"
      case BonusType.Mult  => s"[d${die.min}-${die.max}:+${die.bonusValue}M]"
