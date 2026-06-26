package view

import controller.*
import model.BonusType
import util.Observer

import java.awt.Color
import javax.swing.{BorderFactory, SwingConstants, UIManager}
import scala.swing.*
import scala.swing.event.*

class Gui (controller: IController) extends MainFrame with Observer:

    title = "Cubatro"
    preferredSize = new Dimension(1000, 700)

    private val statusLabel = new Label
    private val bagPanel = new FlowPanel()
    private val handPanel = new FlowPanel()
    private val selectedPanel = new FlowPanel()
    private val inPlayPanel = new FlowPanel()
    private val toRollPanel = new FlowPanel()
    private val currentCombinationValueLabel = new Label("-")
    private val baseScoreValueLabel = new Label("0")
    private val baseMultValueLabel = new Label("0")
    private val lockedRowsArea = new TextArea {
        editable = false
        rows = 8
    }
    private val messageLabel = new Label

    private val actionBackground = new Color(46, 125, 50)
    private val disabledActionBackground = new Color(24, 78, 29)
    private val plainDieBackground = new Color(238, 238, 238)
    private val disabledPlainDieBackground = new Color(88, 88, 88)
    private val chipsBackground = new Color(33, 150, 243)
    private val disabledChipsBackground = new Color(19, 86, 139)
    private val multBackground = new Color(229, 57, 53)
    private val disabledMultBackground = new Color(132, 32, 30)
    private val enabledLightForeground = Color.WHITE
    private val enabledDarkForeground = Color.BLACK
    private val disabledForeground = new Color(220, 220, 220)
    private val descriptionForeground = new Color(80, 80, 80)
    private val panelBorderColor = new Color(120, 120, 120)

    UIManager.put("Button.disabledText", disabledForeground)

    private val discardButton = new Button("Discard")
    private val playButton = new Button("Play")
    private val rerollButton = new Button("Reroll")
    private val scoreButton = new Button("Score")
    private val undoButton = new Button("Undo")
    private val redoButton = new Button("Redo")
    private val saveButton = new Button("Save")
    private val loadButton = new Button("Load")
    private val quitButton = new Button("Quit")

    List(discardButton, playButton, rerollButton, scoreButton, undoButton, redoButton, saveButton, loadButton, quitButton)
        .foreach(styleActionButton)
    addActionTooltips()

    contents = new BorderPanel {
        layout(statusLabel) = BorderPanel.Position.North

        layout(new BoxPanel(Orientation.Vertical) {
            contents += sectionHeader("Bag", "Dice still available to draw after discards.")
            contents += bagPanel
            contents += sectionHeader("Hand", "Dice you can select this turn.")
            contents += handPanel
            contents += sectionHeader("Selected", "Dice chosen to play or discard.")
            contents += selectedPanel
            contents += sectionHeader("In Play", "Rolled dice that form the current combination.")
            contents += inPlayPanel
            contents += sectionHeader("To Roll", "Picked dice waiting for the next reroll.")
            contents += toRollPanel
            contents += scoreInfoPanel
            contents += messageLabel
        }) = BorderPanel.Position.Center

        layout(actionPanel) = BorderPanel.Position.South
    }

    controller.add(this)
    update()

    private def actionPanel: FlowPanel = 

        listenTo(discardButton, playButton, rerollButton, scoreButton, undoButton, redoButton, saveButton, loadButton, quitButton)

            reactions += {
                case ButtonClicked(`discardButton`) => handle(GameCommand.Discard)
                case ButtonClicked(`playButton`)    => handle(GameCommand.PlaySelected)
                case ButtonClicked(`rerollButton`)  => handle(GameCommand.Reroll)
                case ButtonClicked(`scoreButton`)   => handle(GameCommand.ScoreCurrent)
                case ButtonClicked(`undoButton`)    => handle(GameCommand.Undo)
                case ButtonClicked(`redoButton`)    => handle(GameCommand.Redo)
                case ButtonClicked(`saveButton`)    => handle(GameCommand.Save(controller.defaultSavePath))
                case ButtonClicked(`loadButton`)    => handle(GameCommand.Load(controller.defaultSavePath))
                case ButtonClicked(`quitButton`)    => handle(GameCommand.Quit); close()
            }

        new FlowPanel(
            discardButton,
            playButton,
            rerollButton,
            scoreButton,
            undoButton,
            redoButton,
            saveButton,
            loadButton,
            quitButton
        )

    private def sectionHeader(title: String, description: String): BoxPanel =
        val descriptionLabel = new Label(description)
        descriptionLabel.foreground = descriptionForeground

        new BoxPanel(Orientation.Vertical) {
            contents += centeredLabel(new Label(title))
            contents += centeredLabel(descriptionLabel)
        }

    private def centeredLabel(label: Label): FlowPanel =
        label.peer.setHorizontalAlignment(SwingConstants.CENTER)
        val panel = new FlowPanel(label)
        panel.peer.setAlignmentX(java.awt.Component.CENTER_ALIGNMENT)
        panel

    private def scoreInfoPanel: GridPanel =
        new GridPanel(1, 2) {
            contents += new BoxPanel(Orientation.Vertical) {
                contents += sectionHeader("Current Combination", "Best current combo before die bonuses.")
                contents += currentCombinationPanel
            }
            contents += new BoxPanel(Orientation.Vertical) {
                contents += sectionHeader("Scored Rows", "Combinations you already scored.")
                contents += new ScrollPane(lockedRowsArea)
            }
        }

    private def currentCombinationPanel: BoxPanel =
        new BoxPanel(Orientation.Vertical) {
            peer.setBorder(BorderFactory.createLineBorder(panelBorderColor))
            contents += currentCombinationValueLabel
            contents += new Label("BaseScore x BaseMult")
            contents += new FlowPanel(
                valuePanel(baseScoreValueLabel),
                new Label("x"),
                valuePanel(baseMultValueLabel)
            )
        }

    private def valuePanel(label: Label): BorderPanel =
        label.peer.setHorizontalAlignment(SwingConstants.CENTER)
        new BorderPanel {
            preferredSize = new Dimension(80, 45)
            peer.setBorder(BorderFactory.createLineBorder(panelBorderColor))
            layout(label) = BorderPanel.Position.Center
        }

    private def handle(command: GameCommand): Unit =
        controller.handle(command) match
            case Left(error) => messageLabel.text = error
            case Right(_) =>
                val state = controller.viewState
                if state.isWin then messageLabel.text = "You win."
                else if state.isLose then messageLabel.text = "You lose."
                else messageLabel.text = successMessage(command).getOrElse("")

    private def successMessage(command: GameCommand): Option[String] =
        command match
            case GameCommand.Save(path) => Some(s"Game saved to $path.")
            case GameCommand.Load(path) => Some(s"Game loaded from $path.")
            case _                      => None

    private def addActionTooltips(): Unit =
        discardButton.tooltip =
            "Discard the selected dice, spend one discard, and draw replacement dice into your hand."
        playButton.tooltip =
            "Roll the selected dice into play. Then pick dice to reroll or score the current combination."
        rerollButton.tooltip =
            "Reroll the dice in To Roll, spend one reroll, and return them to In Play."
        scoreButton.tooltip =
            "Score the current In Play dice combination, add it to Scored Rows, and spend one play."
        undoButton.tooltip =
            "Undo the last move that changed the game state."
        redoButton.tooltip =
            "Redo the last move that was undone."
        saveButton.tooltip =
            s"Save the current game to ${controller.defaultSavePath}."
        loadButton.tooltip =
            s"Load the saved game from ${controller.defaultSavePath}."
        quitButton.tooltip =
            "Quit the game and close the window."

    override def update(): Unit =
        Swing.onEDT {
            val state = controller.viewState

            statusLabel.text =
                s"Target: ${state.targetScore} | Score: ${state.score} | Phase: ${state.phase} | Plays: ${state.plays} | Rerolls: ${state.rerolls} | Discards: ${state.discards}"

            updateDicePanel(
                bagPanel,
                state.bagDice,
                _ => None,
                die => s"This die is still in the bag and may be drawn later. ${die.tooltip}."
            )
            updateDicePanel(
                handPanel,
                state.handDice,
                index => Some(GameCommand.Select(List(index))),
                die => s"Select this die and move it from Hand to Selected. ${die.tooltip}."
            )
            updateDicePanel(
                selectedPanel,
                state.selectedDiceViews,
                _ => None,
                die => s"This die is selected and will be played or discarded. ${die.tooltip}."
            )
            updateDicePanel(
                inPlayPanel,
                state.inPlayDice,
                index => Some(GameCommand.Pick(List(index))),
                die => s"Pick this rolled die for reroll and move it to To Roll. ${die.tooltip}."
            )
            updateDicePanel(
                toRollPanel,
                state.toRollDice,
                _ => None,
                die => s"This die is waiting to be rerolled. Press Reroll to roll it again. ${die.tooltip}."
            )

            currentCombinationValueLabel.text = state.currentCombination
            baseScoreValueLabel.text = state.currentBaseChips.toString
            baseMultValueLabel.text = state.currentBaseMult.toString

            lockedRowsArea.text =
                if state.lockedRows.isEmpty then "-"
                else state.lockedRows.mkString("\n")

            setActionButtonEnabled(discardButton, state.phase == "Select")
            setActionButtonEnabled(playButton, state.phase == "Select")
            setActionButtonEnabled(rerollButton, state.phase == "PickOut")
            setActionButtonEnabled(scoreButton, state.phase == "PickOut" || state.phase == "Score")
            setActionButtonEnabled(undoButton, true)
            setActionButtonEnabled(redoButton, true)
            setActionButtonEnabled(saveButton, true)
            setActionButtonEnabled(loadButton, true)

            if state.isWin then 
                messageLabel.text = "You win."
                disableGameButtons()
            else if state.isLose then 
                messageLabel.text = "You lose."
                disableGameButtons()

            peer.revalidate()
            peer.repaint()
        }

    private def updateDicePanel(
        panel: FlowPanel,
        dice: List[DieView],
        commandForIndex: Int => Option[GameCommand],
        tooltipForDie: DieView => String
    ): Unit =
        panel.contents.clear()

        dice.zipWithIndex.foreach { case (die, index) =>
            val button = new Button(die.guiText)
            button.tooltip = tooltipForDie(die)
            val command = commandForIndex(index)
            setDieButtonEnabled(button, die, command.isDefined)
            panel.contents += button

            command match
                case Some(command) =>
                    listenTo(button)
                    reactions += {
                        case ButtonClicked(`button`) => handle(command)
                    }
                case None => ()
        }

        panel.peer.revalidate()
        panel.peer.repaint()

    private def styleActionButton(button: Button): Unit =
        button.peer.setOpaque(true)
        button.peer.setContentAreaFilled(true)
        setActionButtonEnabled(button, true)

    private def setActionButtonEnabled(button: Button, isEnabled: Boolean): Unit =
        button.enabled = isEnabled
        button.background = if isEnabled then actionBackground else disabledActionBackground
        button.foreground = if isEnabled then enabledLightForeground else disabledForeground

    private def setDieButtonEnabled(button: Button, die: DieView, isEnabled: Boolean): Unit =
        button.peer.setOpaque(true)
        button.peer.setContentAreaFilled(true)
        button.enabled = isEnabled
        button.background = dieBackground(die, isEnabled)
        button.foreground =
            if isEnabled && die.bonusType == BonusType.None then enabledDarkForeground
            else if isEnabled then enabledLightForeground
            else disabledForeground

    private def dieBackground(die: DieView, isEnabled: Boolean): Color =
        die.bonusType match
            case BonusType.Chips =>
                if isEnabled then chipsBackground else disabledChipsBackground
            case BonusType.Mult =>
                if isEnabled then multBackground else disabledMultBackground
            case BonusType.None =>
                if isEnabled then plainDieBackground else disabledPlainDieBackground

    private def disableGameButtons(): Unit =

        setActionButtonEnabled(discardButton, false)
        setActionButtonEnabled(playButton, false)
        setActionButtonEnabled(rerollButton, false)
        setActionButtonEnabled(scoreButton, false)
        setActionButtonEnabled(undoButton, false)
        setActionButtonEnabled(redoButton, false)

object Gui:
    val launcher: IController => Unit = start

    def start(controller: IController): Unit =
        Swing.onEDT {
            val gui = new Gui(controller)
            gui.visible = true
        }
