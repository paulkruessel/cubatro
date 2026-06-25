package fileio

import model.*
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

class JsonFileIO extends FileIO:

  override def save(gameState: GameState, path: String): Unit =
    Files.writeString(Path.of(path), toJson(gameState), StandardCharsets.UTF_8)

  override def load(path: String): GameState =
    fromJson(Files.readString(Path.of(path), StandardCharsets.UTF_8))

  private def toJson(gameState: GameState): String =
    s"""{
       |  "bag": ${dieListToJson(gameState.bag)},
       |  "availableDice": ${dieListToJson(gameState.availableDice)},
       |  "maxAvailableDice": ${gameState.maxAvailableDice},
       |  "selectedDice": ${dieListToJson(gameState.selectedDice)},
       |  "diceInPlay": ${rolledDieListToJson(gameState.diceInPlay)},
       |  "diceToRoll": ${rolledDieListToJson(gameState.diceToRoll)},
       |  "lockedRows": ${lockedRowsToJson(gameState.lockedRows)},
       |  "cupgrades": ${cupgradesToJson(gameState.cupgrades)},
       |  "discards": ${gameState.discards},
       |  "rerolls": ${gameState.rerolls},
       |  "totalRerolls": ${gameState.totalRerolls},
       |  "plays": ${gameState.plays},
       |  "targetScore": ${gameState.targetScore},
       |  "score": ${gameState.score},
       |  "phase": "${gameState.phase}"
       |}""".stripMargin

  private def fromJson(text: String): GameState =
    GameState(
      bag = parseDieList(section(text, "bag")),
      availableDice = parseDieList(section(text, "availableDice")),
      maxAvailableDice = number(text, "maxAvailableDice"),
      selectedDice = parseDieList(section(text, "selectedDice")),
      diceInPlay = parseRolledDieList(section(text, "diceInPlay")),
      diceToRoll = parseRolledDieList(section(text, "diceToRoll")),
      lockedRows = parseLockedRows(section(text, "lockedRows")),
      cupgrades = parseCupgrades(section(text, "cupgrades")),
      discards = number(text, "discards"),
      rerolls = number(text, "rerolls"),
      totalRerolls = number(text, "totalRerolls"),
      plays = number(text, "plays"),
      targetScore = number(text, "targetScore"),
      score = topLevelNumber(text, "score"),
      phase = string(text, "phase")
        .pipe(Phase.valueOf)
    )

  private def dieToJson(die: Die): String =
    s"""{"min":${die.min},"max":${die.max},"bonusType":"${die.bonusType}","bonusValue":${die.bonusValue}}"""

  private def rolledDieToJson(rolledDie: RolledDie): String =
    s"""{"die":${dieToJson(rolledDie.die)},"value":${rolledDie.value}}"""

  private def lockedRowToJson(row: LockedRow): String =
    s"""{"dice":${rolledDieListToJson(row.dice)},"combination":"${row.combination}","score":${row.score}}"""

  private def cupgradeToJson(cupgrade: Cupgrade): String =
    s"""{"name":"${cupgrade.name}"}"""

  private def dieListToJson(dice: List[Die]): String =
    dice.map(dieToJson).mkString("[", ",", "]")

  private def rolledDieListToJson(dice: List[RolledDie]): String =
    dice.map(rolledDieToJson).mkString("[", ",", "]")

  private def lockedRowsToJson(rows: List[LockedRow]): String =
    rows.map(lockedRowToJson).mkString("[", ",", "]")

  private def cupgradesToJson(cupgrades: List[Cupgrade]): String =
    cupgrades.map(cupgradeToJson).mkString("[", ",", "]")

  private def parseDieList(text: String): List[Die] =
    objects(text).map(parseDie)

  private def parseRolledDieList(text: String): List[RolledDie] =
    objects(text).map(parseRolledDie)

  private def parseLockedRows(text: String): List[LockedRow] =
    objects(text).map(parseLockedRow)

  private def parseCupgrades(text: String): List[Cupgrade] =
    objects(text).map(obj => Cupgrade(string(obj, "name"), identity))

  private def parseDie(text: String): Die =
    Die(
      min = number(text, "min"),
      max = number(text, "max"),
      bonusType = BonusType.valueOf(string(text, "bonusType")),
      bonusValue = number(text, "bonusValue")
    )

  private def parseRolledDie(text: String): RolledDie =
    RolledDie(
      die = parseDie(objectSection(text, "die")),
      value = number(text, "value")
    )

  private def parseLockedRow(text: String): LockedRow =
    LockedRow(
      dice = parseRolledDieList(section(text, "dice")),
      combination = Combination.valueOf(string(text, "combination")),
      score = number(text, "score")
    )

  private def topLevelNumber(text: String, key: String): Int =
    val body = text.dropWhile(_ != '{').drop(1).takeWhile(_ != '\n')
    number(text.substring(text.lastIndexOf(s""""$key"""") - 5), key)

  private def number(text: String, key: String): Int =
    val pattern = s""""$key"\\s*:\\s*(-?\\d+)""".r
    pattern.findFirstMatchIn(text).map(_.group(1).toInt).get

  private def string(text: String, key: String): String =
    val pattern = s""""$key"\\s*:\\s*"([^"]*)"""".r
    pattern.findFirstMatchIn(text).map(_.group(1)).get

  private def section(text: String, key: String): String =
    val start = text.indexOf(s""""$key"""")
    val arrayStart = text.indexOf("[", start)
    balanced(text, arrayStart, '[', ']')

  private def objectSection(text: String, key: String): String =
    val start = text.indexOf(s""""$key"""")
    val objectStart = text.indexOf("{", start)
    balanced(text, objectStart, '{', '}')

  private def objects(arrayText: String): List[String] =
    var result = List.empty[String]
    var index = 0

    while index < arrayText.length do
      val start = arrayText.indexOf("{", index)
      if start == -1 then
        index = arrayText.length
      else
        val obj = balanced(arrayText, start, '{', '}')
        result = result :+ obj
        index = start + obj.length

    result

  private def balanced(text: String, start: Int, open: Char, close: Char): String =
    var depth = 0
    var index = start
    var end = start

    while index < text.length do
      if text.charAt(index) == open then depth += 1
      if text.charAt(index) == close then depth -= 1
      if depth == 0 then
        end = index + 1
        index = text.length
      else
        index += 1

    text.substring(start, end)

extension [A](value: A)
  private def pipe[B](f: A => B): B = f(value)
