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
  CombinationStrategies.all
    .filter(_.matches(dice))
    .map(_.combination)
    .sortBy(_.rank)

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
