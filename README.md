[![Coverage Status](https://coveralls.io/repos/github/paulkruessel/cubatro/badge.svg?branch=main)](https://coveralls.io/github/paulkruessel/cubatro?branch=main)

# Cubatro

Cubatro is a small dice-based terminal game written in Scala 3. The project was developed as part of the Software Engineering course and demonstrates automated testing, code coverage, mutation testing, continuous integration, and the integration of design patterns.

## Requirements

- Java 17 or newer
- sbt

## Build and Run

Compile the project:

```bash
sbt compile
```

Run the game:

```bash
sbt run
```

Run the test suite:

```bash
sbt test
```

Run tests with coverage:

```bash
sbt clean coverage test coverageReport
```

Run mutation testing with Stryker4s:

```bash
sbt stryker
```

## Gameplay

The game is played in the terminal. The player selects dice from the hand, plays them, optionally rerolls dice, and then scores the current dice combination.

Common commands:

```text
help / h
quit / q
select 0 1 2
discard / d
play / p
pick 0 1
reroll / r
score / s
```

The game ends when the player reaches the target score or runs out of plays.

## Continuous Integration and Coverage

The project uses GitHub Actions as the CI system. On each push or pull request, the project is built and tested automatically.

Code coverage is generated with scoverage and uploaded to Coveralls. The Coveralls badge at the top of this README shows the current coverage status of the main branch.

## Design Patterns

Task 7 required integrating three or more design patterns to improve the structure and extensibility of the code. This project uses the following patterns.

### 1. Observer Pattern

The Observer Pattern is used to decouple the game logic from the user interface.

`GameController` extends `Observable`. The terminal UI, `Tui`, extends `Observer` and registers itself at the controller. Whenever the game state changes, the controller calls `notifyObservers()`. The UI can then update itself without the controller needing to know how the UI renders the game.

Relevant files:

```text
src/main/scala/util/Observer.scala
src/main/scala/controller/GameController.scala
src/main/scala/view/Tui.scala
```

Why this improves the design:

- The controller does not depend on a concrete UI implementation.
- Additional views, such as a GUI or logging observer, could be added later.
- Game logic and presentation are better separated.

### 2. Factory Method Pattern

The Factory Method Pattern is used for creating dice.

The `DieFactory` object contains factory methods for the different dice types:

```scala
DieFactory.plain
DieFactory.chips(value)
DieFactory.mult(value)
```

The controller uses these factory methods when creating the initial dice bag. This avoids spreading constructor details for different dice types throughout the code.

Relevant files:

```text
src/main/scala/model/DieFactory.scala
src/main/scala/controller/GameController.scala
```

Why this improves the design:

- Dice creation is centralized.
- The controller no longer needs to know all constructor details.
- New dice types or changed default dice values can be introduced more easily.

### 3. Strategy Pattern

The Strategy Pattern is used for detecting dice combinations.

Each scoring combination is represented by a separate strategy that implements the common `CombinationStrategy` trait:

```scala
trait CombinationStrategy:
  def combination: Combination
  def matches(dice: List[RolledDie]): Boolean
```

Examples are:

```text
ThreeOfAKindStrategy
FourOfAKindStrategy
FullHouseStrategy
SmallStraightStrategy
LargeStraightStrategy
YahtzeeStrategy
```

The `matchingCombinations` function no longer contains all combination logic directly. Instead, it asks all registered strategies whether they match the current dice.

Relevant files:

```text
src/main/scala/model/CombinationStrategy.scala
src/main/scala/model/Model.scala
```

Why this improves the design:

- Each combination rule is isolated in its own strategy.
- The matching logic is easier to read and test.
- New combinations can be added by implementing a new strategy and registering it in `CombinationStrategies.all`.
- The code follows the Open/Closed Principle better: existing matching logic does not need to be rewritten for every new combination.

## Testing

The project contains tests for the model, controller, observer system, terminal UI, and main entry point.

Important test areas:

```text
ModelSpec
GameControllerTest
TuiTest
ObserverTest
MainTest
```

The tests cover:

- dice evaluation
- combination detection
- game phase transitions
- command parsing
- terminal rendering
- observer registration and notification
- factory methods
- invalid commands and edge cases

## Mutation Testing

Stryker4s is used to evaluate the strength of the test suite. Mutation testing changes small parts of the production code and checks whether the tests detect those changes.

The tests were improved based on Stryker4s results. This means the mutation score was improved by adding stronger tests, not by excluding important mutations.

Run mutation testing with:

```bash
sbt stryker
```

## Project Structure

```text
src/main/scala
├── Main.scala
├── controller
│   └── GameController.scala
├── model
│   ├── Model.scala
│   ├── DieFactory.scala
│   └── CombinationStrategy.scala
├── util
│   └── Observer.scala
└── view
    └── Tui.scala

src/test/scala
├── MainTest.scala
├── GameControllerTest.scala
├── ModelTest.scala
├── ObserverTest.scala
└── TuiTest.scala
```

## Summary

The project fulfills the design pattern task by integrating and documenting at least three patterns:

```text
Observer Pattern
Factory Method Pattern
Strategy Pattern
```

These patterns make the code more modular, easier to extend, and easier to test.