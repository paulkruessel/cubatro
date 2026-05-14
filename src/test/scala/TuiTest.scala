import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import controller.*
import model.*
import view.*
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream

class TuiTest extends AnyWordSpec with Matchers {

  private val plain = Die(bonusType = BonusType.None, bonusValue = 0)
  private val chip = Die(bonusType = BonusType.Chips, bonusValue = 2)

  private def setState(controller: GameController, state: GameState): Unit =
    val field = classOf[GameController].getDeclaredField("currentState")
    field.setAccessible(true)
    field.set(controller, state)

  private def state(
      available: List[Die] = Nil,
      selected: List[Die] = Nil,
      inPlay: List[RolledDie] = Nil,
      diceToRoll: List[RolledDie] = Nil,
      rows: List[LockedRow] = Nil,
      phase: Phase = Phase.Select
  ): GameState =
    GameState(
      bag = List.fill(10)(plain),
      availableDice = available,
      maxAvailableDice = 8,
      selectedDice = selected,
      diceInPlay = inPlay,
      diceToRoll = diceToRoll,
      lockedRows = rows,
      cupgrades = Nil,
      discards = 4,
      rerolls = 4,
      totalRerolls = 4,
      plays = 6,
      targetScore = 1000,
      score = 0,
      phase = phase
    )

  private def tuiWithState(gameState: GameState): (GameController, Tui) =
    val controller = new GameController()
    setState(controller, gameState)
    val tui = new Tui(controller, () => "quit", _ => ())
    (controller, tui)

  "Tui" should {

    "cover default input and output" in {
      val input = new ByteArrayInputStream("quit\n".getBytes("UTF-8"))
      val output = new ByteArrayOutputStream()

      Console.withIn(input) {
        Console.withOut(output) {
          val tui = new Tui(new GameController())
          tui.run()
        }
      }

      output.toString should include("Game stopped by player")
    }

    "render empty sections exactly" in {
      val (controller, tui) = tuiWithState(state())
      val output = tui.render(controller.viewState)

      output should include("Hand:\n-")
      output should include("Selected:\n-")
      output should include("In Play:\n-")
      output should include("To Roll:\n-")
      output should include("Locked rows:\n-")
    }

    "render non-empty sections exactly" in {
      val row = LockedRow(
        dice = List(RolledDie(plain, 6)),
        combination = Combination.Sixes,
        score = 43
      )

      val (controller, tui) =
        tuiWithState(
          state(
            available = List(plain, chip),
            selected = List(chip),
            inPlay = List(RolledDie(plain, 5)),
            diceToRoll = List(RolledDie(plain, 4)),
            rows = List(row)
          )
        )

      val output = tui.render(controller.viewState)

      output should include("Hand:\n0:[d1-6] 1:[d1-6:+2C]")
      output should include("Selected:\n0:[d1-6:+2C]")
      output should include("In Play:\n0:[5]")
      output should include("To Roll:\n0:[4]")
      output should include("Locked rows:\n1. Sixes -> 43")
    }

    "parse all long and short commands" in {
      val tui = new Tui(new GameController(), () => "quit", _ => ())

      tui.parse("help") shouldBe GameCommand.Help
      tui.parse("h") shouldBe GameCommand.Help
      tui.parse("quit") shouldBe GameCommand.Quit
      tui.parse("q") shouldBe GameCommand.Quit
      tui.parse("discard") shouldBe GameCommand.Discard
      tui.parse("d") shouldBe GameCommand.Discard
      tui.parse("play") shouldBe GameCommand.PlaySelected
      tui.parse("p") shouldBe GameCommand.PlaySelected
      tui.parse("reroll") shouldBe GameCommand.Reroll
      tui.parse("r") shouldBe GameCommand.Reroll
      tui.parse("score") shouldBe GameCommand.ScoreCurrent
      tui.parse("s") shouldBe GameCommand.ScoreCurrent
      tui.parse("select 0, 1 x 2") shouldBe GameCommand.Select(List(0, 1, 2))
      tui.parse("pick 0 x 1") shouldBe GameCommand.Pick(List(0, 1))
      tui.parse("anything") shouldBe GameCommand.Help
      tui.parse("") shouldBe GameCommand.Help
    }

    "show prompts for all phases" in {
      val tui = new Tui(new GameController(), () => "quit", _ => ())

      tui.prompt("Select") shouldBe "\nSelect phase: select <indices> | discard | play | help | quit\n> "
      tui.prompt("PickOut") shouldBe "\nPickOut phase: pick <indices> | reroll | score | help | quit\n> "
      tui.prompt("Score") shouldBe "\nScore phase: score | help | quit\n> "
      tui.prompt("Draw") shouldBe "\nPhase Draw\n> "
    }

    "show help for all phases" in {
      val tui = new Tui(new GameController(), () => "quit", _ => ())

      tui.help("Select") shouldBe "Select dice with: select 0 1 2. Then use: play."
      tui.help("PickOut") shouldBe "Use: pick 0, then reroll. Or use: score."
      tui.help("Score") shouldBe "Use: score."
      tui.help("Draw") shouldBe "Commands: help, quit."
    }

    "update by rendering current view state" in {
      val controller = new GameController()
      setState(controller, state(available = List(plain)))
      val outputs = scala.collection.mutable.ListBuffer.empty[String]
      val tui = new Tui(controller, () => "quit", text => outputs += text)

      tui.update()

      outputs.mkString should include("CUBATRO")
      outputs.mkString should include("0:[d1-6]")
      outputs.mkString should endWith("\n")
    }

    "run and quit" in {
      val outputs = scala.collection.mutable.ListBuffer.empty[String]
      val tui = new Tui(new GameController(), () => "quit", text => outputs += text)

      tui.run()

      outputs.mkString should include("Game stopped by player")
    }

    "run help then quit" in {
      val inputs = scala.collection.mutable.Queue("help", "quit")
      val outputs = scala.collection.mutable.ListBuffer.empty[String]
      val tui = new Tui(new GameController(), () => inputs.dequeue(), text => outputs += text)

      tui.run()

      outputs.mkString should include("Select dice with: select 0 1 2. Then use: play.")
      outputs.mkString should include("Game stopped by player")
    }

    "run invalid command then quit" in {
      val inputs = scala.collection.mutable.Queue("reroll", "quit")
      val outputs = scala.collection.mutable.ListBuffer.empty[String]
      val tui = new Tui(new GameController(), () => inputs.dequeue(), text => outputs += text)

      tui.run()

      outputs.mkString should include("Action error: Allowed: select, discard, play, help, quit")
    }

    "run valid non terminal command then quit" in {
      val inputs = scala.collection.mutable.Queue("select 0", "quit")
      val outputs = scala.collection.mutable.ListBuffer.empty[String]
      val tui = new Tui(new GameController(), () => inputs.dequeue(), text => outputs += text)

      tui.run()

      outputs.mkString should include("Game stopped by player")
    }

    "run until win and stop" in {
      val controller = new GameController()
      setState(
        controller,
        state(
          inPlay = List(RolledDie(plain, 6), RolledDie(plain, 6)),
          phase = Phase.PickOut
        ).copy(score = 1000)
      )

      val outputs = scala.collection.mutable.ListBuffer.empty[String]
      val tui = new Tui(controller, () => "score", text => outputs += text)

      tui.run()

      outputs.mkString should include("You win.")
    }

    "run until lose and stop" in {
      val controller = new GameController()
      setState(
        controller,
        state(
          inPlay = List(RolledDie(plain, 6), RolledDie(plain, 6)),
          phase = Phase.PickOut
        ).copy(plays = 1, targetScore = 999999)
      )

      val outputs = scala.collection.mutable.ListBuffer.empty[String]
      val tui = new Tui(controller, () => "score", text => outputs += text)

      tui.run()

      outputs.mkString should include("You lose.")
    }
  }
}
