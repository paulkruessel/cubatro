package controller.internal_controller

import model.GameState

trait Command:
  def doStep(): Unit
  def undoStep(): Unit
  def redoStep(): Unit

class GameStateCommand(
    beforeState: GameState,
    afterState: GameState,
    setState: GameState => Unit
) extends Command:

  override def doStep(): Unit =
    setState(afterState)

  override def undoStep(): Unit =
    setState(beforeState)

  override def redoStep(): Unit =
    setState(afterState)
