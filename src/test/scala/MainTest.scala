import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers._

class MainTests extends AnyWordSpec {
  "Main" should {
    "print the full game UI" in {
      val out = new java.io.ByteArrayOutputStream()
      Console.withOut(out) {
      hello()
      }
      val printed = out.toString("UTF-8")

      printed should include("CUBATRO")
      printed should include("Level Score")
      printed should include("Cupgrades")
      printed should include("Locked Rows")
      printed should include("Chips x Mult")
      printed should include("Draft Row")
      printed should include("Rolled Dice")
      printed should include("Hand")
    }

    "contain CUBATRO in titleRow" in {
      titleRow should include("CUBATRO")
    }

    "have borders in titleRow" in {
      titleRow should include("+-----")
    }

    "contain Level Score in scoreBoard" in {
      scoreBoard should include("Level Score")
    }

    "contain Current Score in scoreBoard" in {
      scoreBoard should include("Current Score")
    }

    "contain Cupgrades in upgradeBoard" in {
      upgradeBoard should include("Cupgrades")
    }

    "list upgrades in upgradeBoard" in {
      upgradeBoard should include("Leather")
      upgradeBoard should include("Steel")
      upgradeBoard should include("Pocket Dimension")
      upgradeBoard should include("Blue Sticker")
    }

    "contain Locked Rows in lockedRows" in {
      lockedRows should include("Locked Rows")
    }

    "contain Chips x Mult in draftScore" in {
      draftScore should include("Chips x Mult")
    }

    "contain Draft Row in draftRow" in {
      draftRow should include("Draft Row")
    }

    "contain Rolled Dice in rolledDice" in {
      rolledDice should include("Rolled Dice")
    }

    "contain Hand in diceHand" in {
      diceHand should include("Hand")
    }

    "return non-empty strings for all sections" in {
      titleRow.nonEmpty shouldBe true
      scoreBoard.nonEmpty shouldBe true
      upgradeBoard.nonEmpty shouldBe true
      lockedRows.nonEmpty shouldBe true
      draftScore.nonEmpty shouldBe true
      draftRow.nonEmpty shouldBe true
      rolledDice.nonEmpty shouldBe true
      diceHand.nonEmpty shouldBe true
    }
  }
}
