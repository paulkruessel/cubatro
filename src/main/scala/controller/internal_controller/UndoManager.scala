package controller.internal_controller
import controller.internal_controller.Command

class UndoManager:
  private var undoStack: List[Command] = Nil
  private var redoStack: List[Command] = Nil

  def doStep(command: Command): Unit =
    command.doStep()
    undoStack = command :: undoStack
    redoStack = Nil

  def undoStep(): Option[Command] =
    undoStack match
      case Nil => None
      case command :: remainingCommands =>
        command.undoStep()
        undoStack = remainingCommands
        redoStack = command :: redoStack
        Some(command)

  def redoStep(): Option[Command] =
    redoStack match
      case Nil => None
      case command :: remainingCommands =>
        command.redoStep()
        redoStack = remainingCommands
        undoStack = command :: undoStack
        Some(command)
