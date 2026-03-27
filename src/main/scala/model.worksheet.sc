// Idee: Würfelbased Roguelike:
// Der Spieler kann seinen Becher zusammenbauen und mit verschiedenen Boni upgraden. Er hat bis zu 4 Bonusslots
// Zudem hat der Spieler einen Würfelsack. Würfel kann er modifizieren oder upgraden.
// Der Spieler muss mit den Würfeln Kniffelreihen hinbekommen
// Jede Kniffelreihe hat einen Basevalue und Basemult
// Der Becher und die Würfel erhöhen diesen.
// Rundenablauf:
// Jede Runde bekommt der Spieler eine Anzahl an Würfeln, welche er zufällig aus dem Sack zieht
// Er hat dann die Möglichkeit, einige wegzuschmeißen oder auf das Spielfeld zu ziehen.
// Dann kann der Spieler mit bis zu fünf Würfeln würfeln
// Der Spieler kann auch Würfel zur seite legen
// Am ende kann der Spieler Würfel zählen und Punkte bekommen
// Punkte werden berechnet mit Chips * Mult
// Es gibt Basischips durch die Kombinationen, Chips durch den Becher und Chips durch die Würfel
// Es gibt Basemult durch die Kombinationen, Mult durch den Becher und Mult durch die Würfel

enum BonusType:
  case Chips, Mult, Money, None

enum Combination:
  case Ones, Twos, Threes, Fours, Fives, Sixes
  case ThreeOfAKind, FourOfAKind, FullHouse
  case SmallStraight, LargeStraight
  case Yahtzee, Chance

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
    bonusValue: Int = 0
)

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
    bag: List[Die],                     // Alle Würfel des Spielers -> quasi das Kartendeck
    availableDice: List[Die],           // Die vom Spieler gezogenen und verfügbaren Würfel
    rolledDice: List[RolledDie],        // Gewürfelte Würfel, welche der Spieler auf dem Spielfeld hat und aktuell mit diesen Interagiert
    draftRow: List[Die],                // Eine noch unfertige Würfelreihe, an welcher der Spieler gerade arbeitet
    lockedRows: List[LockedRow],        // Die festgelegten Würfelreihen, welche bereits gezählt wurden
    cupgrades: List[Cupgrade],          // Upgrades des Würfelbechers, welche on top beim Scoren einer fertig gestellten Würfelphase extra Boni geben
    rerollsLeft: Int,                   // Anzahl, wie oft der Spieler noch Würfel wegschmeißen und neu ziehen kann
    targetScore: Int,                   // Score, welchen der Spieler erreichen muss, um das Level zu schaffen
    money: Int,                         // Geld des Spielers. Kann im Shop für upgrades ausgegeben werden
    usedCombinations: Set[Combination], // Bereits genutzte Kombinationen, welche nicht mehr erneut gewertet werden können
    phase: Phase                        // Aktuelle Phase des Spiels
)

