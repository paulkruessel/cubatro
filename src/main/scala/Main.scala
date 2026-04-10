@main def hello(): Unit =
  print(titleRow)
  print(scoreBoard)
  print(upgradeBoard)
  print(lockedRows)
  print(draftScore)
  print(draftRow)
  print(rolledDice)
  print(diceHand)

def titleRow =
  "+---------------------------------------------+\n" +
  "|                   CUBATRO                   |\n" +
  "+---------------------------------------------+\n"
def scoreBoard = 
  "| Level Score: 5000       Current Score: 3000 |\n" +
  "| Rerolls left: 2                 Money: $200 |\n" +
  "+---------------------------------------------+\n"
def upgradeBoard = 
  "|                  Cupgrades:                 |\n" +
  "| - Leather: +2 Mult                          |\n" +
  "| - Steel: +10 Chips                          |\n" +
  "| - Pocket Dimension: +2 Dice for Scoring     |\n" +
  "| - Blue Sticker: x2 all blue dice            |\n" +
  "+---------------------------------------------+\n"
def lockedRows =
  "|                 Locked Rows                 |\n" +
  "| [1][1][1][1][1]: 1000 [1][2][3][4][1]: 2000 |\n" +
  "| [ ][ ][ ][ ][ ]: 0    [ ][ ][ ][ ][ ]: 0    |\n" +
  "+---------------------------------------------+\n"

def draftScore = 
  "|                 Chips x Mult                |\n" +
  "|                                             |\n" +
  "|                [5000] X [5000]              |\n" +
  "|                                             |\n" +
  "+---------------------------------------------+\n"

def draftRow = 
  "|                  Draft Row                  |\n" +
  "|                                             |\n" +
  "|                 [2][3][5][ ][ ]             |\n" +
  "|                                             |\n" +
  "+---------------------------------------------+\n"

def rolledDice = 
  "|                  Rolled Dice                |\n" +
  "|                                             |\n" +
  "|                     [1][6]                  |\n" +
  "|                                             |\n" +
  "+---------------------------------------------+\n"

def diceHand = 
  "|                     Hand                    |\n" +
  "|                                             |\n" +
  "| [1] [1] [1] [1] [1] [1] [1] [1] [1] [1] [1] |\n" +
  "| [1] [1] [1] [1] [1] [1] [1] [1] [1] [1] [1] |\n" +
  "| [1] [1] [1] [1] [1] [1] [1] [1] [1] [1] [1] |\n" +
  "|                                             |\n" +
  "+---------------------------------------------+\n"
