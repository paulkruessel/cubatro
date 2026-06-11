package view

import controller.*
import model.*
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import java.awt.{Component as AwtComponent, Container as AwtContainer, GraphicsEnvironment}
import javax.swing.{JButton, JLabel, JTextArea, SwingUtilities}
import scala.swing.*

class GuiTest extends AnyWordSpec with Matchers:

  private val plainDie = Die(1, 6, BonusType.None, 0)
  private val chipDie = Die(1, 6, BonusType.Chips, 2)
  private val fixedDie = Die(4, 4, BonusType.None, 0)

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

  private def flushEdt(): Unit =
    SwingUtilities.invokeAndWait(() => ())

  private def allAwt(component: AwtComponent): Seq[AwtComponent] =
    val children =
      component match
        case container: AwtContainer =>
          container.getComponents.toSeq.flatMap(allAwt)
        case _ =>
          Seq.empty

    component +: children

  private def labels(gui: Gui): Seq[JLabel] =
    allAwt(gui.peer).collect { case label: JLabel => label }

  private def buttons(gui: Gui): Seq[JButton] =
    allAwt(gui.peer).collect { case button: JButton => button }

  private def textAreas(gui: Gui): Seq[JTextArea] =
    allAwt(gui.peer).collect { case textArea: JTextArea => textArea }

  private def button(gui: Gui, text: String): JButton =
    buttons(gui).find(_.getText == text).getOrElse(
      fail(s"Button not found: $text. Existing buttons: ${buttons(gui).map(_.getText).mkString(", ")}")
    )

  private def firstButtonStartingWith(gui: Gui, prefix: String): JButton =
    buttons(gui).find(_.getText.startsWith(prefix)).getOrElse(
      fail(s"Button starting with '$prefix' not found. Existing buttons: ${buttons(gui).map(_.getText).mkString(", ")}")
    )

  private def click(button: JButton): Unit =
    SwingUtilities.invokeAndWait(() => button.doClick())
    flushEdt()

  private def withGui(testCode: (GameController, Gui) => Unit): Unit =
    assume(!GraphicsEnvironment.isHeadless, "Skipping Swing GUI test in headless environment.")

    val controller = new GameController()
    controller.start()

    var gui: Gui = null

    SwingUtilities.invokeAndWait(() => {
      gui = new Gui(controller)
      gui.update()
    })
    flushEdt()

    try
      testCode(controller, gui)
    finally
      SwingUtilities.invokeAndWait(() => gui.close())

  private def withRawGui(testCode: (GameController, Gui) => Unit): Unit =
    assume(!GraphicsEnvironment.isHeadless, "Skipping Swing GUI test in headless environment.")

    val controller = new GameController()
    controller.start()

    var gui: Gui = null

    SwingUtilities.invokeAndWait(() => {
      gui = new Gui(controller)
    })
    flushEdt()

    try
      testCode(controller, gui)
    finally
      SwingUtilities.invokeAndWait(() => gui.close())

  "Gui" should {

    "render the initial view state" in withGui { (controller, gui) =>
      val statusText = labels(gui).map(_.getText).find(_.contains("Target: 1000")).getOrElse("")

      statusText should include("Target: 1000")
      statusText should include("Score: 0")
      statusText should include("Phase: Select")
      statusText should include("Plays: 6")
      statusText should include("Rerolls: 4")
      statusText should include("Discards: 4")

      buttons(gui).count(_.getText.matches("\\d+:\\[d1-6.*")) shouldBe controller.viewState.hand.size
      textAreas(gui).head.getText shouldBe "-"
    }

    "initialize itself by rendering immediately and keep rows area read-only" in withRawGui { (controller, gui) =>
    val labelTexts = labels(gui).map(_.getText)

    labelTexts.exists(_.contains("Target: 1000")) shouldBe true
    labelTexts.exists(_.contains("Phase: Select")) shouldBe true

    buttons(gui).count(_.getText.matches("\\d+:\\[d1-6.*")) shouldBe controller.viewState.hand.size
    textAreas(gui).head.isEditable shouldBe false
    }

    "enable only Select phase action buttons" in withGui { (_, gui) =>
      button(gui, "Discard").isEnabled shouldBe true
      button(gui, "Play").isEnabled shouldBe true
      button(gui, "Reroll").isEnabled shouldBe false
      button(gui, "Score").isEnabled shouldBe false
      button(gui, "Undo").isEnabled shouldBe true
      button(gui, "Redo").isEnabled shouldBe true
    }

    "select a hand die when a hand button is clicked" in withGui { (controller, gui) =>
      val firstDieButton = firstButtonStartingWith(gui, "0:[d")

      click(firstDieButton)

      controller.state.selectedDice.size shouldBe 1
      controller.state.availableDice.size shouldBe 7

      buttons(gui).exists(_.getText.startsWith("0:[d")) shouldBe true
    }

    "play selected dice when Play is clicked" in withGui { (controller, gui) =>
      controller.handle(GameCommand.Select(List(0)))
      gui.update()
      flushEdt()

      click(button(gui, "Play"))

      controller.state.phase shouldBe Phase.PickOut
      controller.state.selectedDice shouldBe Nil
      controller.state.diceInPlay should not be empty
    }

    "discard selected dice when Discard is clicked" in withGui { (controller, gui) =>
      click(firstButtonStartingWith(gui, "0:[d"))

      click(button(gui, "Discard"))

      controller.state.selectedDice shouldBe Nil
      controller.state.discards shouldBe 3
    }

    "enable PickOut phase buttons correctly" in withGui { (controller, gui) =>
      setState(
        controller,
        state(
          phase = Phase.PickOut,
          diceInPlay = List(RolledDie(fixedDie, 4), RolledDie(chipDie, 5))
        )
      )

      gui.update()
      flushEdt()

      button(gui, "Discard").isEnabled shouldBe false
      button(gui, "Play").isEnabled shouldBe false
      button(gui, "Reroll").isEnabled shouldBe true
      button(gui, "Score").isEnabled shouldBe true
    }

    "enable Score phase buttons correctly" in withGui { (controller, gui) =>
      setState(
        controller,
        state(
          phase = Phase.Score,
          diceInPlay = List(RolledDie(fixedDie, 4)),
          diceToRoll = Nil
        )
      )

      gui.update()
      flushEdt()

      button(gui, "Score").isEnabled shouldBe true
      button(gui, "Reroll").isEnabled shouldBe false
    }

    "move an in-play die to to-roll when it is clicked" in withGui { (controller, gui) =>
        setState(
            controller,
            state(
            phase = Phase.PickOut,
            availableDice = Nil,
            diceInPlay = List(RolledDie(fixedDie, 4), RolledDie(plainDie, 2)),
            diceToRoll = Nil
            )
        )

        gui.update()
        flushEdt()

        val inPlayButton =
            buttons(gui)
            .find(_.getText == "0:[4]")
            .getOrElse(fail(s"In-play button not found. Existing buttons: ${buttons(gui).map(_.getText).mkString(", ")}"))

        click(inPlayButton)

        controller.state.diceInPlay.size shouldBe 1
        controller.state.diceToRoll.size shouldBe 1
        controller.state.diceToRoll.head.value shouldBe 4
        button(gui, "Score").isEnabled shouldBe true
    }

    "reroll selected to-roll dice when Reroll is clicked" in withGui { (controller, gui) =>
      setState(
        controller,
        state(
          phase = Phase.PickOut,
          diceInPlay = Nil,
          diceToRoll = List(RolledDie(fixedDie, 4)),
          rerolls = 4
        )
      )

      gui.update()
      flushEdt()

      click(button(gui, "Reroll"))

      controller.state.phase shouldBe Phase.PickOut
      controller.state.rerolls shouldBe 3
      controller.state.diceToRoll shouldBe Nil
      controller.state.diceInPlay.size shouldBe 1
    }

    "score current dice when Score is clicked" in withGui { (controller, gui) =>
      setState(
        controller,
        state(
          phase = Phase.PickOut,
          diceInPlay = List.fill(5)(RolledDie(plainDie, 6)),
          targetScore = 10000,
          plays = 6
        )
      )

      gui.update()
      flushEdt()

      click(button(gui, "Score"))

      controller.state.score should be > 0
      controller.state.lockedRows should not be empty
      controller.state.plays shouldBe 5
      controller.state.phase shouldBe Phase.Select
    }

    "show locked rows in the text area" in withGui { (controller, gui) =>
      val row = LockedRow(
        dice = List(RolledDie(plainDie, 6)),
        combination = Combination.Sixes,
        score = 43
      )

      setState(
        controller,
        state(
          phase = Phase.Select,
          lockedRows = List(row)
        )
      )

      gui.update()
      flushEdt()

      val text = textAreas(gui).head.getText
      text should include("Sixes")
      text should include("43")
    }

    "show win message and disable game buttons" in withGui { (controller, gui) =>
      setState(
        controller,
        state(
          phase = Phase.Win,
          score = 1000,
          targetScore = 1000
        )
      )

      gui.update()
      flushEdt()

      labels(gui).map(_.getText) should contain("You win.")
      button(gui, "Discard").isEnabled shouldBe false
      button(gui, "Play").isEnabled shouldBe false
      button(gui, "Reroll").isEnabled shouldBe false
      button(gui, "Score").isEnabled shouldBe false
      button(gui, "Undo").isEnabled shouldBe false
      button(gui, "Redo").isEnabled shouldBe false
    }

    "show lose message and disable game buttons" in withGui { (controller, gui) =>
      setState(
        controller,
        state(
          phase = Phase.Lose,
          plays = 0,
          score = 0,
          targetScore = 1000
        )
      )

      gui.update()
      flushEdt()

      labels(gui).map(_.getText) should contain("You lose.")
      button(gui, "Discard").isEnabled shouldBe false
      button(gui, "Play").isEnabled shouldBe false
      button(gui, "Reroll").isEnabled shouldBe false
      button(gui, "Score").isEnabled shouldBe false
    }

    "keep display-only dice buttons disabled" in withGui { (controller, gui) =>
      setState(
        controller,
        state(
          phase = Phase.Select,
          availableDice = List(chipDie),
          selectedDice = List(plainDie),
          diceToRoll = List(RolledDie(plainDie, 3))
        )
      )

      gui.update()
      flushEdt()

      firstButtonStartingWith(gui, "0:[d1-6:+2C]").isEnabled shouldBe true
      buttons(gui).find(_.getText == "0:[3]").exists(_.isEnabled) shouldBe false
      buttons(gui).find(_.getText == "0:[d1-6]").exists(_.isEnabled) shouldBe false
    }

    "quit through the Quit button" in withGui { (controller, gui) =>
      controller.isRunning shouldBe true

      click(button(gui, "Quit"))

      controller.isRunning shouldBe false
    }

    "undo and redo through the action buttons" in withGui { (controller, gui) =>
      click(firstButtonStartingWith(gui, "0:[d"))
      controller.state.selectedDice.size shouldBe 1

      click(button(gui, "Undo"))

      controller.state.selectedDice shouldBe Nil
      controller.state.availableDice.size shouldBe 8

      click(button(gui, "Redo"))

      controller.state.selectedDice.size shouldBe 1
      controller.state.availableDice.size shouldBe 7
    }

    "render all section labels and window title" in withGui { (_, gui) =>
        gui.title shouldBe "Cubatro"

        val labelTexts = labels(gui).map(_.getText)

        labelTexts should contain("Hand")
        labelTexts should contain("Selected")
        labelTexts should contain("In Play")
        labelTexts should contain("To Roll")
        labelTexts should contain("Locked Rows")
    }

    "render multiple locked rows on separate lines" in withGui { (controller, gui) =>
        val row1 = LockedRow(
            dice = List(RolledDie(plainDie, 6)),
            combination = Combination.Sixes,
            score = 43
        )
        val row2 = LockedRow(
            dice = List(RolledDie(plainDie, 5)),
            combination = Combination.Fives,
            score = 20
        )

        setState(
            controller,
            state(
            phase = Phase.Select,
            lockedRows = List(row1, row2)
            )
        )

        gui.update()
        flushEdt()

        val text = textAreas(gui).head.getText.replace("\r\n", "\n")

        text should include("1. Sixes -> 43\n2. Fives -> 20")
    }

        "clear error message after a successful GUI action" in withGui { (_, gui) =>
        click(button(gui, "Undo"))

        labels(gui).map(_.getText) should contain("Nothing to undo")

        click(firstButtonStartingWith(gui, "0:[d"))

        val labelTexts = labels(gui).map(_.getText)

        labelTexts should contain("")
        labelTexts should not contain "Nothing to undo"
        }
  }