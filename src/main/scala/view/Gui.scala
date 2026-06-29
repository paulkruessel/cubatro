package view

import controller.*
import model.BonusType
import util.Observer

import java.awt.{Color, GridLayout}
import javax.swing.{BorderFactory, SwingConstants, UIManager}
import scala.swing.*
import scala.swing.event.*

class Gui (controller: IController) extends MainFrame with Observer:

    title = "Cubatro"
    preferredSize = new Dimension(1000, 700)

    private val bagPanel = new GridPanel(0, 10) {
        peer.setLayout(new GridLayout(0, 10, 6, 6))
        preferredSize = new Dimension(960, 92)
    }
    private val handPanel = new FlowPanel()
    private val inPlayPanel = new FlowPanel()
    private val targetInfoLabel = new Label("Target: 0")
    private val scoreInfoLabel = new Label("Score: 0")
    private val playsInfoLabel = new Label("Plays: 0")
    private val rerollsInfoLabel = new Label("Rerolls: 0")
    private val discardsInfoLabel = new Label("Discards: 0")
    private val currentCombinationTitleLabel = new Label("Current Combination:")
    private val currentCombinationValueLabel = new Label("-")
    private val baseScoreTitleLabel = new Label("BaseScore x BaseMult")
    private val baseScoreValueLabel = new Label("0")
    private val baseMultValueLabel = new Label("0")
    private val phaseInfoLabel = new Label("Phase: -")
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
    private val playsForeground = new Color(25, 118, 210)
    private val panelBorderColor = new Color(120, 120, 120)
    private val regularDieBorder = BorderFactory.createLineBorder(new Color(0, 0, 0, 0), 3)
    private val highlightedDieBorderColor = new Color(251, 192, 45)
    private val highlightedDieBorder = BorderFactory.createLineBorder(highlightedDieBorderColor, 3)
    private var selectedHandSlots: List[Int] = Nil
    private var pickedInPlaySlots: List[Int] = Nil

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
    styleInfoLabels()

    contents = new BorderPanel {
        layout(new BoxPanel(Orientation.Vertical) {
            contents += sectionHeader("Bag", "Dice still available to draw after discards.")
            contents += bagPanel
            contents += sectionHeader("Hand", "Available dice. Yellow border means selected to play or discard.")
            contents += handPanel
            contents += sectionHeader("In Play", "Rolled dice in your combo. Yellow border means picked for reroll.")
            contents += inPlayPanel
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

    private case class DiceButtonEntry(
        die: DieView,
        command: Option[GameCommand],
        tooltip: String,
        highlighted: Boolean = false,
        onSuccess: () => Unit = () => ()
    )

    private def visibleDiceEntries(
        availableDice: List[DieView],
        highlightedDice: List[DieView],
        rememberedSlots: List[Int],
        commandForAvailableIndex: Int => GameCommand,
        tooltipForAvailableDie: DieView => String,
        tooltipForHighlightedDie: DieView => String,
        onAvailableSuccess: (Int, Int) => Unit
    ): List[DiceButtonEntry] =
        val total = availableDice.length + highlightedDice.length
        val highlightedSlots = normalizedSlots(rememberedSlots, highlightedDice.length, total)
        val highlightedBySlot = highlightedSlots.zip(highlightedDice).toMap
        var availableIndex = 0

        (0 until total).toList.map { slot =>
            highlightedBySlot.get(slot) match
                case Some(die) =>
                    DiceButtonEntry(
                        die,
                        None,
                        tooltipForHighlightedDie(die),
                        highlighted = true
                    )
                case None =>
                    val die = availableDice(availableIndex)
                    val index = availableIndex
                    availableIndex += 1
                    DiceButtonEntry(
                        die,
                        Some(commandForAvailableIndex(index)),
                        tooltipForAvailableDie(die),
                        onSuccess = () => onAvailableSuccess(slot, highlightedDice.length)
                    )
        }

    private def normalizedSlots(slots: List[Int], highlightedCount: Int, total: Int): List[Int] =
        val validSlots = slots.foldLeft(List.empty[Int]) { (kept, slot) =>
            if slot >= 0 && slot < total && !kept.contains(slot) then kept :+ slot
            else kept
        }
        val remembered = validSlots.take(highlightedCount)
        val missing = highlightedCount - remembered.length
        val fallback = (0 until total).reverse.filterNot(remembered.contains).take(missing).reverse

        remembered ++ fallback

    private def rememberSelectedHandSlot(slot: Int, selectedCountBefore: Int): Unit =
        selectedHandSlots =
            if selectedCountBefore == 0 then List(slot)
            else selectedHandSlots.take(selectedCountBefore) :+ slot

    private def rememberPickedInPlaySlot(slot: Int, pickedCountBefore: Int): Unit =
        pickedInPlaySlots =
            if pickedCountBefore == 0 then List(slot)
            else pickedInPlaySlots.take(pickedCountBefore) :+ slot

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
            contents += targetInfoLabel
            contents += scoreInfoLabel
            contents += playsInfoLabel
            contents += rerollsInfoLabel
            contents += discardsInfoLabel
            contents += currentCombinationTitleLabel
            contents += currentCombinationValueLabel
            contents += baseScoreTitleLabel
            contents += new FlowPanel(
                valuePanel(baseScoreValueLabel),
                new Label("x"),
                valuePanel(baseMultValueLabel)
            )
            contents += phaseInfoLabel
        }

    private def valuePanel(label: Label): BorderPanel =
        label.peer.setHorizontalAlignment(SwingConstants.CENTER)
        new BorderPanel {
            preferredSize = new Dimension(80, 45)
            peer.setBorder(BorderFactory.createLineBorder(panelBorderColor))
            layout(label) = BorderPanel.Position.Center
        }

    private def handle(command: GameCommand): Boolean =
        controller.handle(command) match
            case Left(error) =>
                messageLabel.text = error
                false
            case Right(_) =>
                val state = controller.viewState
                if state.isWin then messageLabel.text = "You win."
                else if state.isLose then messageLabel.text = "You lose."
                else messageLabel.text = successMessage(command).getOrElse("")
                true

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
            "Reroll the yellow-highlighted In Play dice and spend one reroll."
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

    private def styleInfoLabels(): Unit =
        List(
            targetInfoLabel,
            scoreInfoLabel,
            playsInfoLabel,
            rerollsInfoLabel,
            discardsInfoLabel,
            currentCombinationTitleLabel,
            currentCombinationValueLabel,
            baseScoreTitleLabel,
            phaseInfoLabel
        ).foreach(_.peer.setAlignmentX(java.awt.Component.LEFT_ALIGNMENT))

        playsInfoLabel.foreground = playsForeground
        rerollsInfoLabel.foreground = actionBackground
        discardsInfoLabel.foreground = multBackground

    override def update(): Unit =
        Swing.onEDT {
            val state = controller.viewState

            targetInfoLabel.text = s"Target: ${state.targetScore}"
            scoreInfoLabel.text = s"Score: ${state.score}"
            playsInfoLabel.text = s"Plays: ${state.plays}"
            rerollsInfoLabel.text = s"Rerolls: ${state.rerolls}"
            discardsInfoLabel.text = s"Discards: ${state.discards}"
            phaseInfoLabel.text = s"Phase: ${state.phase}"

            updateDicePanel(
                bagPanel,
                state.bagDice.map(die =>
                    DiceButtonEntry(
                        die,
                        None,
                        s"This die is still in the bag and may be drawn later. ${die.tooltip}."
                    )
                )
            )
            updateDicePanel(
                handPanel,
                visibleDiceEntries(
                    state.handDice,
                    state.selectedDiceViews,
                    selectedHandSlots,
                    index => GameCommand.Select(List(index)),
                    die => s"Select this die. Selected dice stay in Hand with a yellow border. ${die.tooltip}.",
                    die => s"This die is selected and will be played or discarded. ${die.tooltip}.",
                    rememberSelectedHandSlot
                )
            )
            updateDicePanel(
                inPlayPanel,
                visibleDiceEntries(
                    state.inPlayDice,
                    state.toRollDice,
                    pickedInPlaySlots,
                    index => GameCommand.Pick(List(index)),
                    die => s"Pick this rolled die for reroll. Picked dice stay in In Play with a yellow border. ${die.tooltip}.",
                    die => s"This die is picked for reroll. Press Reroll to roll all yellow-highlighted dice again. ${die.tooltip}.",
                    rememberPickedInPlaySlot
                )
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
        panel: Panel,
        entries: List[DiceButtonEntry]
    ): Unit =
        panel.peer.removeAll()

        entries.foreach { entry =>
            val button = new Button(entry.die.guiText)
            button.tooltip = entry.tooltip
            setDieButtonEnabled(button, entry.die, entry.command.isDefined, entry.highlighted)
            panel.peer.add(button.peer)

            entry.command match
                case Some(command) =>
                    listenTo(button)
                    reactions += {
                        case ButtonClicked(`button`) =>
                            entry.onSuccess()
                            handle(command)
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

    private def setDieButtonEnabled(button: Button, die: DieView, isEnabled: Boolean, highlighted: Boolean = false): Unit =
        button.peer.setOpaque(true)
        button.peer.setContentAreaFilled(true)
        button.enabled = isEnabled
        button.background = dieBackground(die, isEnabled)
        button.peer.setBorder(if highlighted then highlightedDieBorder else regularDieBorder)
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
