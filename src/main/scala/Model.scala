import scala.util.Random

enum BonusType:
  case Chips, Mult, None

enum Combination(val rank: Int, val chips: Int, val mult: Int):
  case Ones          extends Combination(0, 5, 1)
  case Twos          extends Combination(1, 10, 1)
  case Threes        extends Combination(2, 15, 1)
  case Fours         extends Combination(3, 20, 1)
  case Fives         extends Combination(4, 25, 1)
  case Sixes         extends Combination(5, 30, 1)
  case ThreeOfAKind  extends Combination(6, 30, 2)
  case FourOfAKind   extends Combination(7, 60, 4)
  case FullHouse     extends Combination(8, 80, 5)
  case SmallStraight extends Combination(9, 100, 6)
  case LargeStraight extends Combination(10, 120, 8)
  case Yahtzee       extends Combination(11, 200, 12)

def matchingCombinations(dice: List[RolledDie]): List[Combination] =
  val values = dice.map(_.value)
  val counts = values.groupBy(identity).view.mapValues(_.size).toMap
  val distinctValues = values.distinct.sorted

  val upperSection =
    List(
      1 -> Combination.Ones,
      2 -> Combination.Twos,
      3 -> Combination.Threes,
      4 -> Combination.Fours,
      5 -> Combination.Fives,
      6 -> Combination.Sixes
    ).collect {
      case (n, comb) if values.contains(n) => comb
    }

  val hasThreeOfAKind = counts.values.exists(_ >= 3)
  val hasFourOfAKind = counts.values.exists(_ >= 4)
  val hasYahtzee = counts.values.exists(_ == 5)
  val hasFullHouse = counts.values.toList.sorted == List(2, 3)

  def hasStraight(length: Int): Boolean =
    distinctValues
      .sliding(length)
      .exists(window => window.length == length && window.last - window.head == length - 1)

  val lowerSection =
    List(
      Option.when(hasThreeOfAKind)(Combination.ThreeOfAKind),
      Option.when(hasFourOfAKind)(Combination.FourOfAKind),
      Option.when(hasFullHouse)(Combination.FullHouse),
      Option.when(hasStraight(4))(Combination.SmallStraight),
      Option.when(hasStraight(5))(Combination.LargeStraight),
      Option.when(hasYahtzee)(Combination.Yahtzee),
    ).flatten

  (upperSection ++ lowerSection).sortBy(_.rank)

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
):

  def roll(): RolledDie =
    RolledDie(
      die = copy(),
      value = Random.nextInt(max - min + 1) + min
    )

case class RolledDie(
    die: Die,
    value: Int
):
  def eval(): (Int, Int) =
    // Returns a score that is scored by this die
    die.bonusType match
      case BonusType.None => (value, 0)
      case BonusType.Chips => (value + die.bonusValue, 0)
      case BonusType.Mult => (value, die.bonusValue)

case class Cupgrade(
    name: String,
    effect: GameState => GameState
)

case class LockedRow(
    dice: List[RolledDie],
    combination: Combination,
    score: Int
)

case class GameState(
  bag: List[Die],                             // holds players dice not drawn (deck)
  availableDice: List[Die],                   // players hand
  maxAvailableDice: Int,                      // size of players hand
  selectedDice: List[Die],                    // dice selected from hand for the next play
  diceInPlay: List[RolledDie],                // dice currently in play
  diceToRoll: List[RolledDie],                // dice to roll next
  lockedRows: List[LockedRow],                // dice already played and scored
  cupgrades: List[Cupgrade],                  // upgrades of the cup
  discards: Int,                              // number of times, player can discard dice
  rerolls: Int,                               // number of times, dice in play can be rerolled
  totalRerolls: Int,                          // max rerolls
  plays: Int,                                 // Anzahl an locked Rows, die der Spieler spielen kann
  targetScore: Int,                           // score to beat
  score: Int,                                 // current score
  // usedCombinations: Set[Combination],         // used combinations, not implemented yet
  phase: Phase                                // current phase
):
  def drawDice(): GameState =
    val numOfDice = maxAvailableDice - availableDice.length
    val drawCount = math.max(0, math.min(numOfDice, bag.size))
    val shuffledBag = Random.shuffle(bag)
    val drawnDice = shuffledBag.take(drawCount)
    val remainingBag = shuffledBag.drop(drawCount)

    copy(
      bag = remainingBag,
      availableDice = availableDice ++ drawnDice
    )

  def selectDice(selectIndex: List[Int]): GameState = 
    // validate indices
    val validIndices =
      selectIndex.filter(i => i >= 0 && i < availableDice.length).distinct
    val remainingSelectionSlots = 5 - selectedDice.length
    val selectedIndices = validIndices.take(math.max(0, remainingSelectionSlots))

    if selectedIndices.isEmpty then this
    else
      
      val diceToSelect = selectedIndices.map(i => availableDice(i))
      // Remove from highest index first to avoid index shifting.
      val discardedAvailableDice =
        selectedIndices.sorted.reverse.foldLeft(availableDice) { (dice, i) =>
          removeAt(dice, i)
        }
      // add newly selected dice to selectedDice
      val newSelectedDice = selectedDice ++ diceToSelect
      // give out new gamestate with newly drawn dice
      copy(availableDice = discardedAvailableDice, selectedDice = newSelectedDice)

  def discardDice(): GameState =
    if discards <= 0 || selectedDice.isEmpty then this
    else
      // Remove selected dice, consume one discard, and draw new ones.
      copy(selectedDice = List.empty[Die], discards = discards - 1).drawDice()

  def addDiceToPlay(): GameState =
    if selectedDice.isEmpty then this
    else
      // Move selected dice into play
      val selectedDiceRolled = selectedDice.map(d => d.roll())
      val sortedDiceInPlay = (diceInPlay ++ selectedDiceRolled).sortBy(_.value)
      copy(diceInPlay = sortedDiceInPlay, selectedDice = List.empty[Die]).drawDice()

  def selectPlayedDice(selectedIndeces: List[Int]): GameState =
    // This function moves the dice selected by selectedIndeces from diceInPlay to diceToRoll, similarly to selectDice.
    val validIndices =
      selectedIndeces.filter(i => i >= 0 && i < diceInPlay.length).distinct
    if validIndices.isEmpty then this
    else

      val diceToMove = validIndices.map(i => diceInPlay(i))

      // Remove from highest index first to avoid index shifting.
      val remainingDiceInPlay =
        validIndices.sorted.reverse.foldLeft(diceInPlay) { (dice, i) =>
          removeAt(dice, i)
        }

      copy(
        diceInPlay = remainingDiceInPlay,
        diceToRoll = diceToRoll ++ diceToMove
      )

  def rollDice(): GameState =
    if rerolls <= 0 || diceToRoll.isEmpty then this
    else
      val rolledDice = diceToRoll.map(d => d.die.roll())
      val sortedDiceInPlay = (diceInPlay ++ rolledDice).sortBy(_.value)
      copy(diceInPlay = sortedDiceInPlay, diceToRoll = List.empty[RolledDie], rerolls = rerolls - 1)

  def scoreDiceInPlay(): GameState =
    if diceInPlay.isEmpty then this
    else
    // calculate score of finished row
      val diceChips = diceInPlay.map(_.eval()._1).sum
      val diceMult = diceInPlay.map(_.eval()._2).sum
      val scoredCombinations = matchingCombinations(diceInPlay)
      val highestCombination = scoredCombinations(scoredCombinations.length - 1)
      val chipsAfterDice = diceChips + highestCombination.chips
      val multAfterDice = diceMult + highestCombination.mult
      val extraScore = chipsAfterDice * multAfterDice

      // Apply cupgrades
      val stateWithScore = copy(score = score + extraScore)
      val finalState = cupgrades.foldLeft(stateWithScore) { (state, cupgrade) =>
        cupgrade.effect(state)
      }

      // Add locked row to gamestate and clear played dice.
      val newLockedRow = LockedRow(
        combination = highestCombination,
        dice = finalState.diceInPlay,
        score = extraScore,
      )
      finalState.copy(
        diceInPlay = List.empty[RolledDie],
        diceToRoll = List.empty[RolledDie],
        lockedRows = finalState.lockedRows ++ List(newLockedRow),
        rerolls = totalRerolls
      )
