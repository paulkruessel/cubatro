import scala.util.Random

enum BonusType:
  case Chips, Mult, None

enum Combination:
  case Ones, Twos, Threes, Fours, Fives, Sixes
  case ThreeOfAKind, FourOfAKind, FullHouse
  case SmallStraight, LargeStraight
  case Yahtzee, Chance

enum Phase:
  case Draw     // automatic phase, start of the round: players hand gets filled up to max
  case Select   // Player selects dice for his next play out of his hand
  case Roll     // automatic phase: player has selected his dice for the next roll and the dice get rolled
  case PickOut  // Player picks dice to be rolled in his next roll from the dice in active play
  case Score    // Player can't or does not want to reroll his dice and wants to score them, dice get scored
  case EndEval  // round ends either because player is above  target score or has no hands left to play
  case Win      // Player has more points then the target score, show win screen, end program
  case Lose     // Player did not get over target score and can't play any more dice, show lose screen, end program

case class Die(
    min: Int = 1,
    max: Int = 6,
    bonusType: BonusType,
    bonusValue: Int,
    rolledValue: Int = 0,
):

  def roll(die: Die): Die =
    die.copy(rolledValue = Random.nextInt(max - min + 1) + min)

  def eval(state: GameState): (Int, GameState) =
    val value = roll()
    bonusType match
      case BonusType.Chips => (value + bonusValue, state)
      case BonusType.Mult  => (value * bonusValue, state)
      case BonusType.None  => (value, state)

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
    bag: List[Die],                             // holds players dice not drawn (deck)
    availableDice: List[Die],                   // players hand
    selectedDice: List[Die],                    // dice selected from hand for the next play
    diceToRoll: List[RolledDie],                // dice currently in play and selected to be rolled
    draftRow: List[RolledDie],                  // dice currently in play and not selected to be rolled
    lockedRows: List[LockedRow],                // dice already played and scored
    cupgrades: List[Cupgrade],                  // upgrades of the cup
    discards: Int,                              // number of times, player can discard dice
    targetScore: Int,                           // score to beat
    score: Int,                                 // current score
    // usedCombinations: Set[Combination],         // used combinations, not implemented yet
    phase: Phase                                // current phase
  )
