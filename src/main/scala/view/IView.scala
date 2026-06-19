package view

import controller.*
import util.Observer

trait IView extends Observer:
  def run(): Unit
  def parse(input: String): GameCommand
  def render(viewState: GameViewState): String
  def prompt(phase: String): String
  def help(phase: String): String
