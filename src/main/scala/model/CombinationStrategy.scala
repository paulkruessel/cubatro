package model

def straight(dice: List[RolledDie], length: Int): Boolean =
    values(dice).distinct.sorted.sliding(length).exists(w => w.length == length && w.last - w.head == length - 1)

def values(dice: List[RolledDie]): List[Int] =
    dice.map(_.value)

def counts(dice: List[RolledDie]): Iterable[List[Int]] =
    values(dice).groupBy(identity).values

trait CombinationStrategy:
    def combination: Combination
    def matches(dice: List[RolledDie]): Boolean

object OnesStrategy extends CombinationStrategy:
    override val combination: Combination = Combination.Ones
    override def matches(dice: List[RolledDie]): Boolean =
        values(dice).contains(1)

object TwosStrategy extends CombinationStrategy:
    override val combination: Combination = Combination.Twos
    override def matches(dice: List[RolledDie]): Boolean =
        values(dice).contains(2)

object ThreesStrategy extends CombinationStrategy:
    override val combination: Combination = Combination.Threes
    override def matches(dice: List[RolledDie]): Boolean =
        values(dice).contains(3)

object FoursStrategy extends CombinationStrategy:
    override val combination: Combination = Combination.Fours
    override def matches(dice: List[RolledDie]): Boolean =
        values(dice).contains(4)

object FivesStrategy extends CombinationStrategy:
    override val combination: Combination = Combination.Fives
    override def matches(dice: List[RolledDie]): Boolean =
        values(dice).contains(5)

object SixesStrategy extends CombinationStrategy:
    override val combination: Combination = Combination.Sixes
    override def matches(dice: List[RolledDie]): Boolean =
        values(dice).contains(6)



object ThreeOfAKindStrategy extends CombinationStrategy:
    override val combination: Combination = Combination.ThreeOfAKind
    override def matches(dice: List[RolledDie]): Boolean =
        counts(dice).exists(_.size >= 3)

object FourOfAKindStrategy extends CombinationStrategy:
    override val combination: Combination = Combination.FourOfAKind
    override def matches(dice: List[RolledDie]): Boolean =
        counts(dice).exists(_.size >= 4)

object YahtzeeStrategy extends CombinationStrategy:
    override val combination: Combination = Combination.Yahtzee
    override def matches(dice: List[RolledDie]): Boolean =
        counts(dice).exists(_.size == 5)

object SmallStraightStrategy extends CombinationStrategy:
    override val combination: Combination = Combination.SmallStraight
    override def matches(dice: List[RolledDie]): Boolean =
        straight(dice, 4)

object LargeStraightStrategy extends CombinationStrategy:
    override val combination: Combination = Combination.LargeStraight
    override def matches(dice: List[RolledDie]): Boolean =
        straight(dice, 5)

object FullHouseStrategy extends CombinationStrategy:
    override val combination: Combination = Combination.FullHouse
    override def matches(dice: List[RolledDie]): Boolean =
        values(dice).groupBy(identity).values.toList.sorted == List(2, 3)

object CombinationStrategies:
  val all: List[CombinationStrategy] =
    List(
      OnesStrategy,
      TwosStrategy,
      ThreesStrategy,
      FoursStrategy,
      FivesStrategy,
      SixesStrategy,
      ThreeOfAKindStrategy,
      FourOfAKindStrategy,
      FullHouseStrategy,
      SmallStraightStrategy,
      LargeStraightStrategy,
      YahtzeeStrategy
    )


