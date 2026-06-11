package controller

import model.*
import util.Observer

trait IController:
  def state: GameState
  def viewState: GameViewState

  def isRunning: Boolean
  def isRunning_=(value: Boolean): Unit

  def start(): Unit
  def handle(command: GameCommand): Either[String, GameState]

  def drawDice(oldState: GameState): GameState
  def selectDice(oldState: GameState, indices: List[Int]): GameState
  def discardDice(oldState: GameState): GameState
  def addDiceToPlay(oldState: GameState): GameState
  def selectPlayedDice(oldState: GameState, indices: List[Int]): GameState
  def rollDice(oldState: GameState): GameState
  def scoreDiceInPlay(oldState: GameState): GameState

  def add(observer: Observer): Unit
  def remove(observer: Observer): Unit