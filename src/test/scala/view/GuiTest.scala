package view

import controller.*
import model.*
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import java.awt.{Color, Component as AwtComponent, Container as AwtContainer, GraphicsEnvironment}
import javax.swing.{JButton, JLabel, JTextArea, SwingConstants, SwingUtilities}
import scala.swing.*

class GuiTest extends AnyWordSpec with Matchers:

  private val plainDie = Die(1, 6, BonusType.None, 0)
  private val chipDie = Die(1, 6, BonusType.Chips, 2)
  private val multDie = Die(1, 6, BonusType.Mult, 2)
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

  private def firstEnabledButtonStartingWith(gui: Gui, prefix: String): JButton =
    buttons(gui).find(button => button.isEnabled && button.getText.startsWith(prefix)).getOrElse(
      fail(s"Enabled button starting with '$prefix' not found. Existing buttons: ${buttons(gui).map(_.getText).mkString(", ")}")
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

      buttons(gui).count(button => button.isEnabled && button.getText.matches("\\[d1-6.*")) shouldBe controller.viewState.hand.size
      textAreas(gui).head.getText shouldBe "-"
    }

    "initialize itself by rendering immediately and keep rows area read-only" in withRawGui { (controller, gui) =>
    val labelTexts = labels(gui).map(_.getText)

    labelTexts.exists(_.contains("Target: 1000")) shouldBe true
    labelTexts.exists(_.contains("Phase: Select")) shouldBe true

    buttons(gui).count(button => button.isEnabled && button.getText.matches("\\[d1-6.*")) shouldBe controller.viewState.hand.size
    textAreas(gui).head.isEditable shouldBe false
    }

    "render bag dice as disabled buttons above the hand" in withGui { (controller, gui) =>
      labels(gui).map(_.getText) should contain("Bag")
      labels(gui).map(_.getText) should contain("Dice still available to draw after discards.")

      val bagButtons = buttons(gui).filter(button => !button.isEnabled && button.getText.matches("\\[d1-6.*"))

      bagButtons.size shouldBe controller.viewState.bag.size
      bagButtons.head.getToolTipText should include("still in the bag")
      bagButtons.head.getToolTipText should include("may be drawn later")
    }

    "enable only Select phase action buttons" in withGui { (_, gui) =>
      button(gui, "Discard").isEnabled shouldBe true
      button(gui, "Play").isEnabled shouldBe true
      button(gui, "Reroll").isEnabled shouldBe false
      button(gui, "Score").isEnabled shouldBe false
      button(gui, "Undo").isEnabled shouldBe true
      button(gui, "Redo").isEnabled shouldBe true
      button(gui, "Save").isEnabled shouldBe true
      button(gui, "Load").isEnabled shouldBe true
    }

    "explain action buttons with tooltips" in withGui { (_, gui) =>
      button(gui, "Discard").getToolTipText should include("Discard the selected dice")
      button(gui, "Play").getToolTipText should include("Roll the selected dice into play")
      button(gui, "Reroll").getToolTipText should include("Reroll the dice in To Roll")
      button(gui, "Score").getToolTipText should include("Score the current In Play dice combination")
      button(gui, "Undo").getToolTipText should include("Undo the last move")
      button(gui, "Redo").getToolTipText should include("Redo the last move")
      button(gui, "Save").getToolTipText should include("Save the current game")
      button(gui, "Load").getToolTipText should include("Load the saved game")
      button(gui, "Quit").getToolTipText should include("Quit the game")
    }

    "select a hand die when a hand button is clicked" in withGui { (controller, gui) =>
      val firstDieButton = firstEnabledButtonStartingWith(gui, "[d")

      click(firstDieButton)

      controller.state.selectedDice.size shouldBe 1
      controller.state.availableDice.size shouldBe 7

      buttons(gui).exists(_.getText.startsWith("[d")) shouldBe true
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
      click(firstEnabledButtonStartingWith(gui, "[d"))

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
            .find(_.getText == "[4]")
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

      val text = textAreas(gui).map(_.getText).mkString("\n")
      text should include("Sixes")
      text should include("43")
    }

    "show current combination with base score and base mult labels" in withGui { (controller, gui) =>
      setState(
        controller,
        state(
          phase = Phase.PickOut,
          diceInPlay = List(
            RolledDie(chipDie, 5),
            RolledDie(multDie, 5),
            RolledDie(plainDie, 5)
          )
        )
      )

      gui.update()
      flushEdt()

      val labelTexts = labels(gui).map(_.getText)

      labelTexts should contain("ThreeOfAKind")
      labelTexts should contain("BaseScore x BaseMult")
      labelTexts should contain("45")
      labelTexts should contain("2")
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
          bag = Nil,
          availableDice = List(chipDie),
          selectedDice = List(plainDie),
          diceToRoll = List(RolledDie(plainDie, 3))
        )
      )

      gui.update()
      flushEdt()

      firstButtonStartingWith(gui, "[d1-6:+2C]").isEnabled shouldBe true
      buttons(gui).find(_.getText == "[3]").exists(_.isEnabled) shouldBe false
      buttons(gui).find(_.getText == "[d1-6]").exists(_.isEnabled) shouldBe false
    }

    "color action and bonus dice buttons and expose bonus tooltips" in withGui { (controller, gui) =>
      setState(
        controller,
        state(
          bag = Nil,
          availableDice = List(chipDie, multDie, plainDie),
          selectedDice = List(chipDie),
          diceInPlay = List(RolledDie(chipDie, 5), RolledDie(multDie, 6)),
          diceToRoll = List(RolledDie(multDie, 3))
        )
      )

      gui.update()
      flushEdt()

      button(gui, "Play").getBackground shouldBe new Color(46, 125, 50)
      button(gui, "Save").getBackground shouldBe new Color(46, 125, 50)
      button(gui, "Load").getBackground shouldBe new Color(46, 125, 50)
      button(gui, "Quit").getBackground shouldBe new Color(46, 125, 50)

      val chipsButton = button(gui, "[d1-6:+2C]")
      chipsButton.getBackground shouldBe new Color(33, 150, 243)
      chipsButton.getForeground shouldBe Color.WHITE
      chipsButton.getToolTipText should include("Select this die")
      chipsButton.getToolTipText should include("Bonus: +2 Chips")

      val multButton = button(gui, "[d1-6:+2M]")
      multButton.getBackground shouldBe new Color(229, 57, 53)
      multButton.getToolTipText should include("Select this die")
      multButton.getToolTipText should include("Bonus: +2 Mult")

      val plainButton = button(gui, "[d1-6]")
      plainButton.getBackground shouldBe new Color(238, 238, 238)
      plainButton.getForeground shouldBe Color.BLACK
      plainButton.getToolTipText should include("Select this die")
      plainButton.getToolTipText should include("Bonus: none")

      button(gui, "[5]").getToolTipText should include("Pick this rolled die for reroll")
      button(gui, "[5]").getToolTipText should include("Bonus: +2 Chips")
      button(gui, "[6]").getToolTipText should include("Pick this rolled die for reroll")
      button(gui, "[6]").getToolTipText should include("Bonus: +2 Mult")
      button(gui, "[3]").getToolTipText should include("waiting to be rerolled")
      button(gui, "[3]").getToolTipText should include("Bonus: +2 Mult")
      buttons(gui).map(_.getText) should not contain "0:[5:+2C]"
      buttons(gui).map(_.getText) should not contain "1:[6:+2M]"
      buttons(gui).map(_.getText) should not contain "0:[3:+2M]"
    }

    "style disabled buttons with light text and darker backgrounds" in withGui { (controller, gui) =>
      setState(
        controller,
        state(
          phase = Phase.Select,
          bag = Nil,
          availableDice = List(chipDie),
          selectedDice = List(plainDie),
          diceInPlay = List(RolledDie(chipDie, 5)),
          diceToRoll = List(RolledDie(multDie, 3))
        )
      )

      gui.update()
      flushEdt()

      button(gui, "Reroll").isEnabled shouldBe false
      button(gui, "Reroll").getForeground shouldBe new Color(220, 220, 220)
      button(gui, "Reroll").getBackground shouldBe new Color(24, 78, 29)

      val selectedPlain = button(gui, "[d1-6]")
      selectedPlain.isEnabled shouldBe false
      selectedPlain.getForeground shouldBe new Color(220, 220, 220)
      selectedPlain.getBackground shouldBe new Color(88, 88, 88)

      val toRollMult = button(gui, "[3]")
      toRollMult.isEnabled shouldBe false
      toRollMult.getForeground shouldBe new Color(220, 220, 220)
      toRollMult.getBackground shouldBe new Color(132, 32, 30)
    }

    "quit through the Quit button" in withGui { (controller, gui) =>
      controller.isRunning shouldBe true

      click(button(gui, "Quit"))

      controller.isRunning shouldBe false
    }

    "undo and redo through the action buttons" in withGui { (controller, gui) =>
      click(firstEnabledButtonStartingWith(gui, "[d"))
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

        labelTexts should contain("Bag")
        labelTexts should contain("Dice still available to draw after discards.")
        labelTexts should contain("Hand")
        labelTexts should contain("Dice you can select this turn.")
        labelTexts should contain("Selected")
        labelTexts should contain("Dice chosen to play or discard.")
        labelTexts should contain("In Play")
        labelTexts should contain("Rolled dice that form the current combination.")
        labelTexts should contain("To Roll")
        labelTexts should contain("Picked dice waiting for the next reroll.")
        labelTexts should contain("Current Combination")
        labelTexts should contain("Best current combo before die bonuses.")
        labelTexts should contain("BaseScore x BaseMult")
        labelTexts should contain("Scored Rows")
        labelTexts should contain("Combinations you already scored.")

        val descriptionLabels = Seq(
          "Dice still available to draw after discards.",
          "Dice you can select this turn.",
          "Dice chosen to play or discard.",
          "Rolled dice that form the current combination.",
          "Picked dice waiting for the next reroll.",
          "Best current combo before die bonuses.",
          "Combinations you already scored."
        ).map(text => labels(gui).find(_.getText == text).getOrElse(fail(s"Label not found: $text")))

        descriptionLabels.foreach { label =>
          label.getForeground shouldBe new Color(80, 80, 80)
          label.getHorizontalAlignment shouldBe SwingConstants.CENTER
        }
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

        val text = textAreas(gui).map(_.getText).mkString("\n").replace("\r\n", "\n")

        text should include("1. Sixes -> 43\n2. Fives -> 20")
    }

        "clear error message after a successful GUI action" in withGui { (_, gui) =>
        click(button(gui, "Undo"))

        labels(gui).map(_.getText) should contain("Nothing to undo")

        click(firstEnabledButtonStartingWith(gui, "[d"))

        val labelTexts = labels(gui).map(_.getText)

        labelTexts should contain("")
        labelTexts should not contain "Nothing to undo"
        }
  }
