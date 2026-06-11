package view

import controller.*
import util.Observer

import scala.swing.*
import scala.swing.event.*

class Gui (controller: IController) extends MainFrame with Observer:

    title = "Cubatro"
    preferredSize = new Dimension(1000, 700)

    controller.add(this)

    private val statusLabel = new Label("")
    private val handPanel = new FlowPanel()
    private val selectedPanel = new FlowPanel()
    private val inPlayPanel = new FlowPanel()
    private val toRollPanel = new FlowPanel()
    private val lockedRowsArea = new TextArea {
        editable = false
        rows = 8
    }
    private val messageLabel = new Label("")


    private val discardButton = new Button("Discard")
    private val playButton = new Button("Play")
    private val rerollButton = new Button("Reroll")
    private val scoreButton = new Button("Score")
    private val undoButton = new Button("Undo")
    private val redoButton = new Button("Redo")
    private val quitButton = new Button("Quit")

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

            updateDicePanel(handPanel, state.hand, index => Some(GameCommand.Select(List(index))))
            updateDicePanel(selectedPanel, state.selected, _ => None)
            updateDicePanel(inPlayPanel, state.inPlay, index => Some(GameCommand.Pick(List(index))))
            updateDicePanel(toRollPanel, state.toRoll, _ => None)

            lockedRowsArea.text =
                if state.lockedRows.isEmpty then "-"
                else state.lockedRows.mkString("\n")

            discardButton.enabled = state.phase == "Select"
            playButton.enabled = state.phase == "Select"
            rerollButton.enabled = state.phase == "PickOut"
            scoreButton.enabled = state.phase == "PickOut" || state.phase == "Score"
            undoButton.enabled = true
            redoButton.enabled = true

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
        dice: List[String],
        commandForIndex: Int => Option[GameCommand]
    ): Unit =
        panel.contents.clear()

        dice.zipWithIndex.foreach { case (text, index) =>
            val button = new Button(text)
            panel.contents += button

            commandForIndex(index) match
                case Some(command) =>
                    listenTo(button)
                    reactions += {
                        case ButtonClicked(`button`) => handle(command)
                    }
                case None =>
                    button.enabled = false
        }

        panel.peer.revalidate()
        panel.peer.repaint()

    private def disableGameButtons(): Unit =

        discardButton.enabled = false
        playButton.enabled = false
        rerollButton.enabled = false
        scoreButton.enabled = false
        undoButton.enabled = false
        redoButton.enabled = false