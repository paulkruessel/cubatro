class MainTests extends munit.FunSuite {
  test("titleRow should contain CUBATRO") {
    assert(titleRow.contains("CUBATRO"))
  }

  test("titleRow should have borders") {
    assert(titleRow.contains("+-----"))
  }

  test("scoreBoard should contain Level Score") {
    assert(scoreBoard.contains("Level Score"))
  }

  test("scoreBoard should contain Current Score") {
    assert(scoreBoard.contains("Current Score"))
  }

  test("upgradeBoard should contain Cupgrades") {
    assert(upgradeBoard.contains("Cupgrades"))
  }

  test("upgradeBoard should list upgrades") {
    assert(upgradeBoard.contains("Leather"))
    assert(upgradeBoard.contains("Steel"))
    assert(upgradeBoard.contains("Pocket Dimension"))
    assert(upgradeBoard.contains("Blue Sticker"))
  }

  test("lockedRows should contain Locked Rows") {
    assert(lockedRows.contains("Locked Rows"))
  }

  test("draftScore should contain Chips x Mult") {
    assert(draftScore.contains("Chips x Mult"))
  }

  test("draftRow should contain Draft Row") {
    assert(draftRow.contains("Draft Row"))
  }

  test("rolledDice should contain Rolled Dice") {
    assert(rolledDice.contains("Rolled Dice"))
  }

  test("diceHand should contain Hand") {
    assert(diceHand.contains("Hand"))
  }

  test("all functions should return non-empty strings") {
    assert(titleRow.nonEmpty)
    assert(scoreBoard.nonEmpty)
    assert(upgradeBoard.nonEmpty)
    assert(lockedRows.nonEmpty)
    assert(draftScore.nonEmpty)
    assert(draftRow.nonEmpty)
    assert(rolledDice.nonEmpty)
    assert(diceHand.nonEmpty)
  }
}
