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

    "render empty sections" in {
      val controller = new GameController()
      setState(controller, state())

      val tui = new Tui(controller, () => "quit", _ => ())
      val output = tui.render(controller.viewState)

      output should include("Hand:\n-")
      output should include("Selected:\n-")
      output should include("In Play:\n-")
      output should include("Locked rows:\n-")
    }

    "render non-empty hand selected in play and locked rows" in {
      val controller = new GameController()

      val row = LockedRow(
        dice = List(RolledDie(plain, 6)),
        combination = Combination.Sixes,
        score = 43
      )

      setState(
        controller,
        state(
          available = List(plain, chip),
          selected = List(chip),
          inPlay = List(RolledDie(plain, 5)),
          rows = List(row)
        )
      )

      val output =
        new Tui(controller, () => "quit", _ => ()).render(controller.viewState)

      output should include("0:[d1-6]")
      output should include("1:[d1-6:+2C]")
      output should include("0:[5]")
      output should include("1. Sixes -> 43")
    }

      "render toRoll non-empty" in {
        val controller = new GameController()
        setState(
          controller,
          state(
            inPlay = Nil,
            diceToRoll = List(RolledDie(plain, 4))
          )
        )

        val output = new Tui(controller, () => "quit", _ => ()).render(controller.viewState)

        output should include("To Roll:")
        output should include("0:[4]")
      }

    "parse all commands" in {
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
    }

    "show prompts for all phases" in {
      val tui = new Tui(new GameController(), () => "quit", _ => ())

      tui.prompt("Select") should include("Select phase")
      tui.prompt("PickOut") should include("PickOut phase")
      tui.prompt("Score") should include("Score phase")
      tui.prompt("Draw") should include("Phase Draw")
    }

    "show help for all phases" in {
      val tui = new Tui(new GameController(), () => "quit", _ => ())

      tui.help("Select") should include("Select dice")
      tui.help("PickOut") should include("reroll")
      tui.help("Score") should include("score")
      tui.help("Draw") should include("Commands")
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

      outputs.mkString should include("Select dice")
      outputs.mkString should include("Game stopped by player")
    }

    "run invalid command then quit" in {
      val inputs = scala.collection.mutable.Queue("reroll", "quit")
      val outputs = scala.collection.mutable.ListBuffer.empty[String]
      val tui = new Tui(new GameController(), () => inputs.dequeue(), text => outputs += text)

      tui.run()

      outputs.mkString should include("Action error")
    }

    "run valid non terminal command then quit" in {
      val inputs = scala.collection.mutable.Queue("select 0", "quit")
      val outputs = scala.collection.mutable.ListBuffer.empty[String]

      val tui =
        new Tui(
          new GameController(),
          () => inputs.dequeue(),
          text => outputs += text
        )

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