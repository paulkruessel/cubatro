package fileio

import model.*
import scala.xml.*

class XmlFileIO extends FileIO:

  override val defaultSavePath: String = "cubatro-save.xml"

  override def save(gameState: GameState, path: String): Unit =
    XML.save(path, toXml(gameState), "UTF-8", xmlDecl = true)

  override def load(path: String): GameState =
    fromXml(XML.loadFile(path))

  private def toXml(gameState: GameState): Elem =
    <gameState
      maxAvailableDice={gameState.maxAvailableDice.toString}
      discards={gameState.discards.toString}
      rerolls={gameState.rerolls.toString}
      totalRerolls={gameState.totalRerolls.toString}
      plays={gameState.plays.toString}
      targetScore={gameState.targetScore.toString}
      score={gameState.score.toString}
      phase={gameState.phase.toString}>
      <bag>{gameState.bag.map(dieToXml)}</bag>
      <availableDice>{gameState.availableDice.map(dieToXml)}</availableDice>
      <selectedDice>{gameState.selectedDice.map(dieToXml)}</selectedDice>
      <diceInPlay>{gameState.diceInPlay.map(rolledDieToXml)}</diceInPlay>
      <diceToRoll>{gameState.diceToRoll.map(rolledDieToXml)}</diceToRoll>
      <lockedRows>{gameState.lockedRows.map(lockedRowToXml)}</lockedRows>
      <cupgrades>{gameState.cupgrades.map(c => <cupgrade name={c.name}/>)}</cupgrades>
    </gameState>

  private def fromXml(node: Node): GameState =
    GameState(
      bag = ((node \ "bag") \ "die").map(dieFromXml).toList,
      availableDice = ((node \ "availableDice") \ "die").map(dieFromXml).toList,
      maxAvailableDice = intAttribute(node, "maxAvailableDice"),
      selectedDice = ((node \ "selectedDice") \ "die").map(dieFromXml).toList,
      diceInPlay = ((node \ "diceInPlay") \ "rolledDie").map(rolledDieFromXml).toList,
      diceToRoll = ((node \ "diceToRoll") \ "rolledDie").map(rolledDieFromXml).toList,
      lockedRows = ((node \ "lockedRows") \ "lockedRow").map(lockedRowFromXml).toList,
      cupgrades = ((node \ "cupgrades") \ "cupgrade").map(cupgradeFromXml).toList,
      discards = intAttribute(node, "discards"),
      rerolls = intAttribute(node, "rerolls"),
      totalRerolls = intAttribute(node, "totalRerolls"),
      plays = intAttribute(node, "plays"),
      targetScore = intAttribute(node, "targetScore"),
      score = intAttribute(node, "score"),
      phase = Phase.valueOf(stringAttribute(node, "phase"))
    )

  private def dieToXml(die: Die): Elem =
    <die
      min={die.min.toString}
      max={die.max.toString}
      bonusType={die.bonusType.toString}
      bonusValue={die.bonusValue.toString}/>

  private def dieFromXml(node: Node): Die =
    Die(
      min = intAttribute(node, "min"),
      max = intAttribute(node, "max"),
      bonusType = BonusType.valueOf(stringAttribute(node, "bonusType")),
      bonusValue = intAttribute(node, "bonusValue")
    )

  private def rolledDieToXml(rolledDie: RolledDie): Elem =
    <rolledDie value={rolledDie.value.toString}>
      {dieToXml(rolledDie.die)}
    </rolledDie>

  private def rolledDieFromXml(node: Node): RolledDie =
    RolledDie(
      die = dieFromXml((node \ "die").head),
      value = intAttribute(node, "value")
    )

  private def lockedRowToXml(row: LockedRow): Elem =
    <lockedRow combination={row.combination.toString} score={row.score.toString}>
      <dice>{row.dice.map(rolledDieToXml)}</dice>
    </lockedRow>

  private def lockedRowFromXml(node: Node): LockedRow =
    LockedRow(
      dice = ((node \ "dice") \ "rolledDie").map(rolledDieFromXml).toList,
      combination = Combination.valueOf(stringAttribute(node, "combination")),
      score = intAttribute(node, "score")
    )

  private def cupgradeFromXml(node: Node): Cupgrade =
    Cupgrade(stringAttribute(node, "name"), identity)

  private def stringAttribute(node: Node, name: String): String =
    (node \ s"@$name").text

  private def intAttribute(node: Node, name: String): Int =
    stringAttribute(node, name).toInt
