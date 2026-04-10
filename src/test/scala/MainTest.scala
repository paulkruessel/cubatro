import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class MainTests extends AnyFunSuite with Matchers {
  test("main should print the full game UI") {
    val out = new java.io.ByteArrayOutputStream()
    Console.withOut(out) {
      main()
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

  test("titleRow should contain CUBATRO") {
    titleRow should include("CUBATRO")
  }

  test("titleRow should have borders") {
    titleRow should include("+-----")
  }

  test("scoreBoard should contain Level Score") {
    scoreBoard should include("Level Score")
  }

  test("scoreBoard should contain Current Score") {
    scoreBoard should include("Current Score")
  }

  test("upgradeBoard should contain Cupgrades") {
    upgradeBoard should include("Cupgrades")
  }

  test("upgradeBoard should list upgrades") {
    upgradeBoard should include("Leather")
    upgradeBoard should include("Steel")
    upgradeBoard should include("Pocket Dimension")
    upgradeBoard should include("Blue Sticker")
  }

  test("lockedRows should contain Locked Rows") {
    lockedRows should include("Locked Rows")
  }

  test("draftScore should contain Chips x Mult") {
    draftScore should include("Chips x Mult")
  }

  test("draftRow should contain Draft Row") {
    draftRow should include("Draft Row")
  }

  test("rolledDice should contain Rolled Dice") {
    rolledDice should include("Rolled Dice")
  }

  test("diceHand should contain Hand") {
    diceHand should include("Hand")
  }

  test("all functions should return non-empty strings") {
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
