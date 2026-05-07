package model

import scala.util.Random

def removeAt[A](list: List[A], index: Int): List[A] =
  list.take(index) ++ list.drop(index + 1)

enum BonusType:
  case Chips, Mult, None

enum Combination(val rank: Int, val chips: Int, val mult: Int):
  case Ones extends Combination(0, 5, 1)
  case Twos extends Combination(1, 10, 1)
  case Threes extends Combination(2, 15, 1)
  case Fours extends Combination(3, 20, 1)
  case Fives extends Combination(4, 25, 1)
  case Sixes extends Combination(5, 30, 1)
  case ThreeOfAKind extends Combination(6, 30, 2)
  case FourOfAKind extends Combination(7, 60, 4)
  case FullHouse extends Combination(8, 80, 5)
  case SmallStraight extends Combination(9, 100, 6)
  case LargeStraight extends Combination(10, 120, 8)
  case Yahtzee extends Combination(11, 200, 12)

enum Phase:
  case Draw, Select, Roll, PickOut, Score, EndEval, Win, Lose

case class Die(
    min: Int = 1,
    max: Int = 6,
    bonusType: BonusType,
    bonusValue: Int
):
  def roll(): RolledDie =
    RolledDie(this, Random.nextInt(max - min + 1) + min)

case class RolledDie(die: Die, value: Int):
  def eval(): (Int, Int) =
    die.bonusType match
      case BonusType.None  => (value, 0)
      case BonusType.Chips => (value + die.bonusValue, 0)
      case BonusType.Mult  => (value, die.bonusValue)

case class Cupgrade(name: String, effect: GameState => GameState)

case class LockedRow(
    dice: List[RolledDie],
    combination: Combination,
    score: Int
)

def matchingCombinations(dice: List[RolledDie]): List[Combination] =
  val values = dice.map(_.value)
  val counts = values.groupBy(identity).view.mapValues(_.size).toMap
  val distinctValues = values.distinct.sorted

  val upper =
    List(
      1 -> Combination.Ones,
      2 -> Combination.Twos,
      3 -> Combination.Threes,
      4 -> Combination.Fours,
      5 -> Combination.Fives,
      6 -> Combination.Sixes
    ).collect { case (n, c) if values.contains(n) => c }

  def straight(length: Int): Boolean =
    distinctValues.sliding(length).exists(w => w.length == length && w.last - w.head == length - 1)

  val lower =
    List(
      Option.when(counts.values.exists(_ >= 3))(Combination.ThreeOfAKind),
      Option.when(counts.values.exists(_ >= 4))(Combination.FourOfAKind),
      Option.when(counts.values.toList.sorted == List(2, 3))(Combination.FullHouse),
      Option.when(straight(4))(Combination.SmallStraight),
      Option.when(straight(5))(Combination.LargeStraight),
      Option.when(counts.values.exists(_ == 5))(Combination.Yahtzee)
    ).flatten

  (upper ++ lower).sortBy(_.rank)

case class GameState(
    bag: List[Die],
    availableDice: List[Die],
    maxAvailableDice: Int,
    selectedDice: List[Die],
    diceInPlay: List[RolledDie],
    diceToRoll: List[RolledDie],
    lockedRows: List[LockedRow],
    cupgrades: List[Cupgrade],
    discards: Int,
    rerolls: Int,
    totalRerolls: Int,
    plays: Int,
    targetScore: Int,
    score: Int,
    phase: Phase
)
