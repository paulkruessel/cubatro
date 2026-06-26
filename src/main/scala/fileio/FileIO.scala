package fileio

import model.GameState

trait FileIO:
  def defaultSavePath: String
  def load(path: String): GameState
  def save(gameState: GameState, path: String): Unit
