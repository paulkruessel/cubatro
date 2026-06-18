package view

import controller.*
import model.BonusType
import util.Observer

import java.awt.Color
import javax.swing.UIManager
import scala.swing.*
import scala.swing.event.*

class Gui (controller: IController) extends MainFrame with Observer:

    title = "Cubatro"
    preferredSize = new Dimension(1000, 700)

    private val statusLabel = new Label
    private val handPanel = new FlowPanel()
    private val selectedPanel = new FlowPanel()
    private val inPlayPanel = new FlowPanel()
    private val toRollPanel = new FlowPanel()
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

    UIManager.put("Button.disabledText", disabledForeground)

    private val discardButton = new Button("Discard")
    private val playButton = new Button("Play")
    private val rerollButton = new Button("Reroll")
    private val scoreButton = new Button("Score")
    private val undoButton = new Button("Undo")
    private val redoButton = new Button("Redo")
    private val quitButton = new Button("Quit")

    List(discardButton, playButton, rerollButton, scoreButton, undoButton, redoButton, quitButton)
        .foreach(styleActionButton)

    contents = new BorderPanel {
        layout(statusLabel) = BorderPanel.Position.North

        layout(new BoxPanel(Orientation.Vertical) {
            contents += new Label("Hand")
            contents += handPanel
            contents += new Label("Selected")
            contents += selectedPanel
            contents += new Label("In Play")
            contents += inPlayPanel
            contents += new Label("To Roll")
            contents += toRollPanel
            contents += new Label("Locked Rows")
            contents += new ScrollPane(lockedRowsArea)
            contents += messageLabel
        }) = BorderPanel.Position.Center

        layout(actionPanel) = BorderPanel.Position.South
    }

    controller.add(this)
    update()

    private def actionPanel: FlowPanel = 

        listenTo(discardButton, playButton, rerollButton, scoreButton, undoButton, redoButton, quitButton)

            reactions += {
                case ButtonClicked(`discardButton`) => handle(GameCommand.Discard)
                case ButtonClicked(`playButton`)    => handle(GameCommand.PlaySelected)
                case ButtonClicked(`rerollButton`)  => handle(GameCommand.Reroll)
                case ButtonClicked(`scoreButton`)   => handle(GameCommand.ScoreCurrent)
                case ButtonClicked(`undoButton`)    => handle(GameCommand.Undo)
                case ButtonClicked(`redoButton`)    => handle(GameCommand.Redo)
                case ButtonClicked(`quitButton`)    => handle(GameCommand.Quit); close()
            }

        new FlowPanel(
            discardButton,
            playButton,
            rerollButton,
            scoreButton,
            undoButton,
            redoButton,
            quitButton
        )

    private def handle(command: GameCommand): Unit =
        controller.handle(command) match
            case Left(error) => messageLabel.text = error
            case Right(_) => messageLabel.text = ""

    override def update(): Unit =
        Swing.onEDT {
            val state = controller.viewState

            statusLabel.text =
                s"Target: ${state.targetScore} | Score: ${state.score} | Phase: ${state.phase} | Plays: ${state.plays} | Rerolls: ${state.rerolls} | Discards: ${state.discards}"

            updateDicePanel(handPanel, state.handDice, index => Some(GameCommand.Select(List(index))))
            updateDicePanel(selectedPanel, state.selectedDiceViews, _ => None)
            updateDicePanel(inPlayPanel, state.inPlayDice, index => Some(GameCommand.Pick(List(index))))
            updateDicePanel(toRollPanel, state.toRollDice, _ => None)

            lockedRowsArea.text =
                if state.lockedRows.isEmpty then "-"
                else state.lockedRows.mkString("\n")

            setActionButtonEnabled(discardButton, state.phase == "Select")
            setActionButtonEnabled(playButton, state.phase == "Select")
            setActionButtonEnabled(rerollButton, state.phase == "PickOut")
            setActionButtonEnabled(scoreButton, state.phase == "PickOut" || state.phase == "Score")
            setActionButtonEnabled(undoButton, true)
            setActionButtonEnabled(redoButton, true)

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
        commandForIndex: Int => Option[GameCommand]
    ): Unit =
        panel.contents.clear()

        dice.zipWithIndex.foreach { case (die, index) =>
            val button = new Button(die.guiText)
            button.tooltip = die.tooltip
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
