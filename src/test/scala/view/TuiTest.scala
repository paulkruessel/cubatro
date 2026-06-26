import controller.*
import fileio.XmlFileIO
import model.*
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import view.Tui

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.file.Files
import scala.collection.mutable.ListBuffer
import scala.util.Success

class TuiTest extends AnyWordSpec with Matchers:

  private val plainDie = Die(1, 6, BonusType.None, 0)
  private val chipDie = Die(1, 6, BonusType.Chips, 2)
  private val multDie = Die(1, 6, BonusType.Mult, 2)

  private def normalize(text: String): String =
    text.replace("\r\n", "\n").replace("\r", "\n")

  private def setState(controller: GameController, state: GameState): Unit =
    val field = classOf[GameController].getDeclaredField("currentState")
    field.setAccessible(true)
    field.set(controller, state)

  private def state(
      phase: Phase = Phase.Select,
      bag: List[Die] = List.fill(20)(plainDie),
      availableDice: List[Die] = List.fill(8)(plainDie),
      selectedDice: List[Die] = Nil,
      diceInPlay: List[RolledDie] = Nil,
      diceToRoll: List[RolledDie] = Nil,
      lockedRows: List[LockedRow] = Nil,
      discards: Int = 4,
      rerolls: Int = 4,
      totalRerolls: Int = 4,
      plays: Int = 6,
      targetScore: Int = 1000,
      score: Int = 0
  ): GameState =
    GameState(
      bag = bag,
      availableDice = availableDice,
      maxAvailableDice = 8,
      selectedDice = selectedDice,
      diceInPlay = diceInPlay,
      diceToRoll = diceToRoll,
      lockedRows = lockedRows,
      cupgrades = Nil,
      discards = discards,
      rerolls = rerolls,
      totalRerolls = totalRerolls,
      plays = plays,
      targetScore = targetScore,
      score = score,
      phase = phase
    )

  private def runTuiWithInputs(
      inputs: List[String],
      configure: GameController => Unit = _ => (),
      startController: Boolean = true
  ): String =
    val controller = new GameController()
    val outputs = ListBuffer.empty[String]
    val iterator = inputs.iterator

    val tui = new Tui(
      controller,
      readInput = () => if iterator.hasNext then iterator.next() else "q",
      writeOutput = text => outputs += text
    )

    if startController then controller.start()
    outputs.clear()
    configure(controller)

    tui.run()
    outputs.mkString

  "Tui" should {

    "parse all long and short commands" in {
      val controller = new GameController()
      val tui = new Tui(controller, () => "q", _ => ())

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

      tui.parse("undo") shouldBe GameCommand.Undo
      tui.parse("u") shouldBe GameCommand.Undo

      tui.parse("redo") shouldBe GameCommand.Redo
      tui.parse("save") shouldBe GameCommand.Save(controller.defaultSavePath)
      tui.parse("save Saves/MyGame.JSON") shouldBe GameCommand.Save("Saves/MyGame.JSON")
      tui.parse("load") shouldBe GameCommand.Load(controller.defaultSavePath)
      tui.parse("l") shouldBe GameCommand.Load(controller.defaultSavePath)
      tui.parse("load Saves/MyGame.JSON") shouldBe GameCommand.Load("Saves/MyGame.JSON")

      tui.parse("select 0 1 2") shouldBe GameCommand.Select(List(0, 1, 2))
      tui.parse("select 0,1,2") shouldBe GameCommand.Select(List(0, 1, 2))
      tui.parse("pick 0 2") shouldBe GameCommand.Pick(List(0, 2))

      tui.parse("something unknown") shouldBe GameCommand.Invalid
    }

    "use the injected FileIO default path for save and load commands" in {
      val controller = new GameController(fileIO = new XmlFileIO())
      val tui = new Tui(controller, () => "q", _ => ())

      controller.defaultSavePath shouldBe "cubatro-save.xml"
      tui.parse("save") shouldBe GameCommand.Save("cubatro-save.xml")
      tui.parse("load") shouldBe GameCommand.Load("cubatro-save.xml")
    }

    "parse safely" in {
      val controller = new GameController()
      val tui = new Tui(controller, () => "q", _ => ())

      tui.parseSafe("select 0 1") shouldBe Success(GameCommand.Select(List(0, 1)))
      tui.parseSafe("invalid") shouldBe Success(GameCommand.Invalid)
      tui.parseSafe(null).isFailure shouldBe true
      tui.parseSafe("").isFailure shouldBe true
      tui.parseSafe("select").isFailure shouldBe true
      tui.parseSafe("select nope").isFailure shouldBe true
    }

    "parse index errors with helpful messages" in {
      val controller = new GameController()
      val tui = new Tui(controller, () => "q", _ => ())

      tui.parseSafe("select").failed.get.getMessage shouldBe
        "No indices entered. Use help to see valid commands."

      tui.parseSafe("select nope").failed.get.getMessage shouldBe
        "'nope' is not a valid index. Use whole numbers only."
    }

    "show prompts for all phases" in {
      val controller = new GameController()
      val tui = new Tui(controller, () => "q", _ => ())

      tui.prompt("Select") should include("Select phase")
      tui.prompt("Select") should include("select")
      tui.prompt("Select") should include("save")
      tui.prompt("Select") should include("load")
      tui.prompt("Select") should include("quit")

      tui.prompt("PickOut") should include("PickOut phase")
      tui.prompt("PickOut") should include("reroll")
      tui.prompt("PickOut") should include("score")
      tui.prompt("PickOut") should include("save")
      tui.prompt("PickOut") should include("load")

      tui.prompt("Score") should include("Score phase")
      tui.prompt("Score") should include("score")
      tui.prompt("Score") should include("save")
      tui.prompt("Score") should include("load")

      tui.prompt("Win") should include("Phase Win")
    }

    "show help for all phases" in {
      val controller = new GameController()
      val tui = new Tui(controller, () => "q", _ => ())

      tui.help("Select") should include("Select dice")
      tui.help("Select") should include("Save")
      tui.help("Select") should include("Load")
      tui.help("PickOut") should include("pick")
      tui.help("Score") should include("score")
      tui.help("Win") should include("Commands")
    }

    "render empty sections" in {
      val controller = new GameController()
      val tui = new Tui(controller, () => "q", _ => ())

      val viewState = GameViewState(
        targetScore = 1000,
        score = 0,
        phase = "Select",
        plays = 6,
        rerolls = 4,
        discards = 4,
        hand = Nil,
        selected = Nil,
        inPlay = Nil,
        toRoll = Nil,
        lockedRows = Nil,
        isWin = false,
        isLose = false
      )

      val output = normalize(tui.render(viewState))

      output should include("CUBATRO")
      output should include("Target: 1000")
      output should include("Score: 0")
      output should include("Phase: Select")
      output should include("Hand:\n-")
      output should include("Selected:\n-")
      output should include("In Play:\n-")
      output should include("To Roll:\n-")
      output should include("Locked rows:\n-")
    }

    "render non-empty sections" in {
      val controller = new GameController()
      val tui = new Tui(controller, () => "q", _ => ())

      val viewState = GameViewState(
        targetScore = 1000,
        score = 63,
        phase = "PickOut",
        plays = 5,
        rerolls = 3,
        discards = 4,
        hand = List("0:[d1-6]", "1:[d1-6:+2C]"),
        selected = List("0:[d1-6:+2M]", "1:[d1-6:+2C]"),
        inPlay = List("0:[5]", "1:[6]"),
        toRoll = List("0:[4]", "1:[2]"),
        lockedRows = List("1. Sixes -> 43", "2. Fives -> 20"),
        isWin = false,
        isLose = false
      )

      val output = tui.render(viewState)

      output should include("Target: 1000")
      output should include("Score: 63")
      output should include("Phase: PickOut")
      output should include("0:[d1-6] 1:[d1-6:+2C]")
      output should include("0:[d1-6:+2M] 1:[d1-6:+2C]")
      output should include("0:[5] 1:[6]")
      output should include("0:[4] 1:[2]")
      output should include("1. Sixes -> 43")
      output should include("2. Fives -> 20")
    }

    "update by rendering current view state" in {
      val controller = new GameController()
      val outputs = ListBuffer.empty[String]
      val tui = new Tui(controller, () => "q", text => outputs += text)

      controller.start()

      outputs.mkString should include("CUBATRO")
      outputs.mkString should include("Phase: Select")
      outputs.lastOption.get should endWith("\n")
    }

    "run help then quit" in {
      val output = runTuiWithInputs(List("help", "q"))

      output should include("Select dice with: select 0 1 2")
      output should include("Game stopped by player.")
    }

    "run short help then quit" in {
      val output = runTuiWithInputs(List("h", "q"))

      output should include("Select dice with: select 0 1 2")
      output should include("Game stopped by player.")
    }

    "run invalid command then quit" in {
      val output = runTuiWithInputs(List("nonsense", "q"))

      output should include("Action error: Unknown command. Use help to see valid commands.")
      output should include("Game stopped by player.")
    }

    "run empty command then quit" in {
      val output = runTuiWithInputs(List("", "q"))

      output should include("Action error: No command entered. Use help to see valid commands.")
      output should include("Game stopped by player.")
    }

    "run valid non-terminal command then quit" in {
      val output = runTuiWithInputs(List("select 0", "q"))

      output should include("Selected:")
      output should include("Game stopped by player.")
    }

    "run save command then quit" in {
      val file = Files.createTempFile("cubatro-tui", ".json")

      try
        val output = runTuiWithInputs(List(s"save ${file.toString}", "q"))

        output should include(s"Game saved to ${file.toString}.")
        normalize(output) should include(s"Game saved to ${file.toString}.\n\nSelect phase:")
        output should include("Game stopped by player.")
      finally
        Files.deleteIfExists(file)
    }

    "run load command then quit" in {
      val file = Files.createTempFile("cubatro-tui", ".json")
      val seedController = new GameController()
      seedController.start()
      seedController.handle(GameCommand.Select(List(0)))
      seedController.save(file.toString)

      try
        val output = runTuiWithInputs(List(s"load ${file.toString}", "q"))

        output should include(s"Game loaded from ${file.toString}.")
        output should include("Game stopped by player.")
      finally
        Files.deleteIfExists(file)
    }

    "run and quit" in {
      val output = runTuiWithInputs(List("q"))

      output should include("Game stopped by player.")
      normalize(output) should endWith("Game stopped by player.\n")
    }

    "run with the default input reader" in {
      val input = new ByteArrayInputStream("q\n".getBytes("UTF-8"))
      val controller = new GameController()
      val outputs = ListBuffer.empty[String]

      Console.withIn(input) {
        val tui = new Tui(controller, writeOutput = text => outputs += text)
        controller.start()
        outputs.clear()

        tui.run()
      }

      outputs.mkString should include("Game stopped by player.")
    }

    "run with the default output writer" in {
      val output = new ByteArrayOutputStream()
      val controller = new GameController()

      Console.withOut(output) {
        val tui = new Tui(controller, readInput = () => "q")
        controller.start()

        tui.run()
      }

      output.toString("UTF-8") should include("Game stopped by player.")
    }

    "start a stopped controller before processing input" in {
      val output = runTuiWithInputs(List("q"), startController = false)

      output should include("Select phase")
      output should include("Game stopped by player.")
    }

    "not restart a controller that is already running" in {
      val controller = new GameController()
      controller.isRunning = true
      val outputs = ListBuffer.empty[String]
      val tui = new Tui(controller, () => "q", text => outputs += text)

      tui.run()

      outputs.head should include("Phase Draw")
      outputs.head should not include("Select phase")
    }

    "run until win and stop" in {
      val output = runTuiWithInputs(
        List("score"),
        controller =>
          setState(
            controller,
            state(
              phase = Phase.PickOut,
              diceInPlay = List.fill(5)(RolledDie(plainDie, 6)),
              targetScore = 1,
              plays = 6
            )
          )
      )

      output should include("You win.\n")
      output should include("Phase: Win")
    }

    "run until lose and stop" in {
      val output = runTuiWithInputs(
        List("score"),
        controller =>
          setState(
            controller,
            state(
              phase = Phase.PickOut,
              diceInPlay = List(RolledDie(plainDie, 1)),
              targetScore = 1000,
              plays = 1
            )
          )
      )

      output should include("You lose.\n")
      output should include("Phase: Lose")
    }

    "render hand entries separated by spaces" in {
      val controller = new GameController()
      val tui = new Tui(controller, () => "q", _ => ())

      val viewState = GameViewState(
        targetScore = 1000,
        score = 0,
        phase = "Select",
        plays = 6,
        rerolls = 4,
        discards = 4,
        hand = List("0:[d1-6]", "1:[d1-6:+2C]"),
        selected = Nil,
        inPlay = Nil,
        toRoll = Nil,
        lockedRows = Nil,
        isWin = false,
        isLose = false
      )

      normalize(tui.render(viewState)) should include("0:[d1-6] 1:[d1-6:+2C]")
    }

    "write prompt and help with expected line breaks" in {
      val output = runTuiWithInputs(List("help", "q"))

      normalize(output) should include(
        "Select phase: select <indices> | discard | play | save [path] | load [path] | undo | redo | help | quit\n> Select dice with: select 0 1 2."
      )
      normalize(output) should include("Select dice with: select 0 1 2. Then use: play. Save with: save [path]. Load with: load [path]. Undo with: undo. Redo with: redo.\n")
    }

    "write a blank line between help output and the next prompt" in {
      val output = normalize(runTuiWithInputs(List("help", "q")))

      output should include(
        "Select dice with: select 0 1 2. Then use: play. Save with: save [path]. Load with: load [path]. Undo with: undo. Redo with: redo.\n\nSelect phase:"
      )
    }

    "stop the controller after quit input" in {
      val controller = new GameController()
      val outputs = ListBuffer.empty[String]

      val tui = new Tui(
        controller,
        readInput = () => "q",
        writeOutput = text => outputs += text
      )

      controller.start()
      controller.isRunning shouldBe true

      tui.run()

      controller.isRunning shouldBe false
      normalize(outputs.mkString) should endWith("Game stopped by player.\n")
    }

    "stop immediately after a win without reading another input" in {
      val controller = new GameController()
      val outputs = ListBuffer.empty[String]
      val inputs = ListBuffer("score")

      val tui = new Tui(
        controller,
        readInput = () =>
          if inputs.nonEmpty then inputs.remove(0)
          else fail("TUI tried to read another input after the game was won."),
        writeOutput = text => outputs += text
      )

      controller.start()

      setState(
        controller,
        state(
          phase = Phase.PickOut,
          diceInPlay = List.fill(5)(RolledDie(plainDie, 6)),
          targetScore = 1,
          plays = 6
        )
      )

      tui.run()

      controller.isRunning shouldBe false

      val output = normalize(outputs.mkString)
      output should include("You win.\n")
      output should not include "Game stopped by player."
    }

    "render locked rows separated by a newline" in {
      val controller = new GameController()
      val tui = new Tui(controller, () => "q", _ => ())

      val viewState = GameViewState(
        targetScore = 1000,
        score = 63,
        phase = "Select",
        plays = 5,
        rerolls = 3,
        discards = 4,
        hand = Nil,
        selected = Nil,
        inPlay = Nil,
        toRoll = Nil,
        lockedRows = List("1. Sixes -> 43", "2. Fives -> 20"),
        isWin = false,
        isLose = false
      )

      val output = normalize(tui.render(viewState))

      output should include("Locked rows:\n1. Sixes -> 43\n2. Fives -> 20")
      output should not include "1. Sixes -> 432. Fives -> 20"
    }
  }
