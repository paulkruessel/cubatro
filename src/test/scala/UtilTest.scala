import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers._

class UtilSpec extends AnyWordSpec {
  private val d1 = Die(1, 6, BonusType.None, 0)
  private val d2 = Die(1, 6, BonusType.Chips, 2)
  private val d3 = Die(1, 6, BonusType.Mult, 3)

  "removeAt" should {
    "remove the die at a valid middle index" in {
      val dice = List(d1, d2, d3)

      removeAt(dice, 1) shouldBe List(d1, d3)
    }

    "remove the first die when index is 0" in {
      val dice = List(d1, d2, d3)

      removeAt(dice, 0) shouldBe List(d2, d3)
    }

    "remove the last die when index points to the end" in {
      val dice = List(d1, d2, d3)

      removeAt(dice, 2) shouldBe List(d1, d2)
    }

    "return the original list for a negative index" in {
      val dice = List(d1, d2, d3)
      val result = removeAt(dice, -1)

      result shouldBe dice
      (result eq dice) shouldBe true
    }

    "return the original list for an out-of-range index" in {
      val dice = List(d1, d2, d3)
      val result = removeAt(dice, 3)

      result shouldBe dice
      (result eq dice) shouldBe true
    }

    "return an empty list unchanged" in {
      val empty = List.empty[Die]

      removeAt(empty, 0) shouldBe Nil
    }

    "work with generic list types" in {
      val values = List("a", "b", "c")

      removeAt(values, 1) shouldBe List("a", "c")
    }
  }
}
