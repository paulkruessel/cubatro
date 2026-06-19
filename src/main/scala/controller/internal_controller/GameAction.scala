package controller.internal_controller

import model.GameState

trait UndoableCommand:
  def doStep(): GameState
  def undoStep(): Option[GameState]
  def redoStep(): Option[GameState]