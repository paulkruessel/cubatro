import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import model.*

class ModelSpec extends AnyWordSpec with Matchers {

  private val plainDie = Die(bonusType = BonusType.None, bonusValue = 0)
  private val chipDie = Die(bonusType = BonusType.Chips, bonusValue = 2)
  private val multDie = Die(bonusType = BonusType.Mult, bonusValue = 1)

  "Model" should {

    "have all BonusType enum cases" in {
      BonusType.values.toSet shouldBe Set(BonusType.Chips, BonusType.Mult, BonusType.None)
    }

    "have all Phase enum cases" in {
      Phase.values.toSet shouldBe Set(
        Phase.Draw,
        Phase.Select,
        Phase.Roll,
        Phase.PickOut,
        Phase.Score,
        Phase.EndEval,
        Phase.Win,
        Phase.Lose
      )
    }

    "remove an element at the selected index" in {
      removeAt(List(1, 2, 3, 4), 0) shouldBe List(2, 3, 4)
      removeAt(List(1, 2, 3, 4), 1) shouldBe List(1, 3, 4)
      removeAt(List(1, 2, 3, 4), 2) shouldBe List(1, 2, 4)
      removeAt(List(1, 2, 3, 4), 3) shouldBe List(1, 2, 3)
    }

    "roll die values in fixed range" in {
      val die = Die(min = 4, max = 4, bonusType = BonusType.None, bonusValue = 0)
      die.roll().value shouldBe 4
    }

    "evaluate all die bonus types" in {
      RolledDie(plainDie, 3).eval() shouldBe (3, 0)
      RolledDie(chipDie, 3).eval() shouldBe (5, 0)
      RolledDie(multDie, 3).eval() shouldBe (3, 1)
    }

    "detect upper section combinations" in {
      val result = matchingCombinations(
        List(
          RolledDie(plainDie, 1),
          RolledDie(plainDie, 2),
          RolledDie(plainDie, 3),
          RolledDie(plainDie, 4),
          RolledDie(plainDie, 5),
          RolledDie(plainDie, 6)
        )
      )

      result should contain allOf (
        Combination.Ones,
        Combination.Twos,
        Combination.Threes,
        Combination.Fours,
        Combination.Fives,
        Combination.Sixes
      )
    }

    "detect three of a kind only with exactly enough equal dice" in {
      val result = matchingCombinations(
        List(
          RolledDie(plainDie, 3),
          RolledDie(plainDie, 3),
          RolledDie(plainDie, 3),
          RolledDie(plainDie, 4),
          RolledDie(plainDie, 5)
        )
      )

      result should contain (Combination.ThreeOfAKind)
      result should not contain Combination.FourOfAKind
      result should not contain Combination.Yahtzee
    }

    "not detect three of a kind with only two equal dice" in {
      val result = matchingCombinations(
        List(
          RolledDie(plainDie, 3),
          RolledDie(plainDie, 3),
          RolledDie(plainDie, 4),
          RolledDie(plainDie, 5),
          RolledDie(plainDie, 6)
        )
      )

      result should not contain Combination.ThreeOfAKind
    }

    "detect four of a kind only with exactly enough equal dice" in {
      val result = matchingCombinations(
        List(
          RolledDie(plainDie, 4),
          RolledDie(plainDie, 4),
          RolledDie(plainDie, 4),
          RolledDie(plainDie, 4),
          RolledDie(plainDie, 2)
        )
      )

      result should contain (Combination.ThreeOfAKind)
      result should contain (Combination.FourOfAKind)
      result should not contain Combination.Yahtzee
    }

    "not detect four of a kind with only three equal dice" in {
      val result = matchingCombinations(
        List(
          RolledDie(plainDie, 4),
          RolledDie(plainDie, 4),
          RolledDie(plainDie, 4),
          RolledDie(plainDie, 2),
          RolledDie(plainDie, 3)
        )
      )

      result should contain (Combination.ThreeOfAKind)
      result should not contain Combination.FourOfAKind
    }

    "detect full house exactly" in {
      val result = matchingCombinations(
        List(
          RolledDie(plainDie, 2),
          RolledDie(plainDie, 2),
          RolledDie(plainDie, 3),
          RolledDie(plainDie, 3),
          RolledDie(plainDie, 3)
        )
      )

      result should contain (Combination.FullHouse)
    }

    "not detect full house when counts are different" in {
      val result = matchingCombinations(
        List(
          RolledDie(plainDie, 2),
          RolledDie(plainDie, 2),
          RolledDie(plainDie, 2),
          RolledDie(plainDie, 2),
          RolledDie(plainDie, 3)
        )
      )

      result should not contain Combination.FullHouse
    }

    "detect small straight without large straight" in {
      val result = matchingCombinations(
        List(
          RolledDie(plainDie, 1),
          RolledDie(plainDie, 2),
          RolledDie(plainDie, 3),
          RolledDie(plainDie, 4),
          RolledDie(plainDie, 6)
        )
      )

      result should contain (Combination.SmallStraight)
      result should not contain Combination.LargeStraight
    }

    "detect large straight" in {
      val result = matchingCombinations(
        List(
          RolledDie(plainDie, 1),
          RolledDie(plainDie, 2),
          RolledDie(plainDie, 3),
          RolledDie(plainDie, 4),
          RolledDie(plainDie, 5)
        )
      )

      result should contain (Combination.SmallStraight)
      result should contain (Combination.LargeStraight)
    }

    "not detect straight when values have gaps" in {
      val result = matchingCombinations(
        List(
          RolledDie(plainDie, 1),
          RolledDie(plainDie, 2),
          RolledDie(plainDie, 4),
          RolledDie(plainDie, 5),
          RolledDie(plainDie, 6)
        )
      )

      result should not contain Combination.SmallStraight
      result should not contain Combination.LargeStraight
    }

    "detect yahtzee exactly" in {
      val result = matchingCombinations(List.fill(5)(RolledDie(plainDie, 6)))

      result should contain (Combination.ThreeOfAKind)
      result should contain (Combination.FourOfAKind)
      result should contain (Combination.Yahtzee)
    }

    "detect yahtzee when only one value appears five times" in {
      val result = matchingCombinations(
        List.fill(5)(RolledDie(plainDie, 6)) :+ RolledDie(plainDie, 5)
      )

      result should contain (Combination.Yahtzee)
    }

    "not detect yahtzee with only four equal dice" in {
      val result = matchingCombinations(
        List(
          RolledDie(plainDie, 6),
          RolledDie(plainDie, 6),
          RolledDie(plainDie, 6),
          RolledDie(plainDie, 6),
          RolledDie(plainDie, 5)
        )
      )

      result should contain (Combination.FourOfAKind)
      result should not contain Combination.Yahtzee
    }
  }
}
