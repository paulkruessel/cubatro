import scala.util.Random

enum BonusType:
  case Chips, Mult, Money, None

enum Combination:
  case Ones, Twos, Threes, Fours, Fives, Sixes
  case ThreeOfAKind, FourOfAKind, FullHouse
  case SmallStraight, LargeStraight
  case Yahtzee, Chance

enum ConsoleColors(val code: String):
  case CLEAR extends ConsoleColors("\u001B[0m")
  case RED extends ConsoleColors("\u001B[31m")
  case GREEN extends ConsoleColors("\u001B[32m")
  case YELLOW extends ConsoleColors("\u001B[33m")
  case BLUE extends ConsoleColors("\u001B[34m")
  case PURPLE extends ConsoleColors("\u001B[35m")
  case CYAN extends ConsoleColors("\u001B[36m")
  case WHITE extends ConsoleColors("\u001B[37m")

  def apply(text: String): String = s"$code$text${CLEAR.code}"

enum Phase:
  case Draw
  case Select
  case Roll
  case Lock
  case Score
  case Shop

case class Die(
    min: Int = 1,
    max: Int = 6,
    bonusType: BonusType,
    bonusValue: Int
):
  def roll(): Int = Random.nextInt(max - min + 1) + min

  def eval(state: GameState): (Int, GameState) = bonusType match
    case BonusType.None  => (roll(), state)
    case BonusType.Chips => (roll() + bonusValue, state)
    case BonusType.Mult  => (roll() * bonusValue, state)
    case BonusType.Money => (roll(), state.addMoney(bonusValue))

case class RolledDie(
    die: Die,
    value: Int
)

case class Score(
    chips: Int,
    mult: Int
):
  def total: Int = chips * mult

case class Cupgrade(
    name: String,
    effect: GameState => GameState
)

case class LockedRow(
    dice: List[RolledDie],
    combination: Combination,
    score: Score
)

case class GameState(
    bag: List[Die],
    availableDice: List[Die],
    rolledDice: List[RolledDie],
    draftRow: List[RolledDie],
    lockedRows: List[LockedRow],
    cupgrades: List[Cupgrade],
    rerollsLeft: Int,
    targetScore: Int,
    money: Int,
    usedCombinations: Set[Combination],
    phase: Phase
):
  def addMoney(moneyAdded: Int): GameState =
    copy(money = money + moneyAdded)
