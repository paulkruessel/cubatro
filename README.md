[![Coverage Status](https://coveralls.io/repos/github/paulkruessel/cubatro/badge.svg?branch=main)](https://coveralls.io/github/paulkruessel/cubatro?branch=main)

# Cubatro

Cubatro is a small dice game written in Scala 3. The project is part of the Software Engineering course and demonstrates components, dependency injection, design patterns, GUI/TUI integration, automated tests, code coverage, Coveralls and mutation testing with Stryker4s.

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

The player selects dice from the hand, plays them, optionally rerolls dice and then scores the current dice combination.

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
undo / u
redo
```

The game ends when the player reaches the target score or runs out of plays.

## Notes for the Next Submission

The next submission covers the previous missing points and the new dependency injection task.

Implemented for task 10, Components:

- Controller and view access are encapsulated behind interfaces.
- The controller is exposed through `IController`.
- Views use the controller interface instead of depending on concrete controller internals.
- The TUI and GUI are observers of the controller and can operate together.

Implemented for task 11, Dependency Injection:

- `di.AppModule` defines the component bindings.
- `di.DefaultAppModule` binds `IController` to `GameController` and `IView` to `Tui`.
- `di.AppInjector` creates component instances through the module.
- `Main.scala` creates the injector and receives the controller and view from it.
- The GUI startup is delegated to `Gui.start`, so Swing-specific behavior stays in the GUI component.

Fixed from the previous submission feedback:

- Added another design pattern: the State Pattern is used for automatic phase transitions in `PhaseState.scala`.
- The project uses the Scala `Try` monad with capital `T` in `Tui.parseSafe`.
- Bonus dice are visible in every phase, including dice that are already in play or selected for reroll.
- Bonus dice buttons are colored by bonus type: blue for Chips and red for Mult.
- Dice buttons show a tooltip with bonus type and bonus value.
- The bottom UI action buttons are green.

Quality requirements:

- All tests pass.
- `MainTest` and `GuiTest` are excluded from the default sbt test run.
- scoverage reports 100% statement coverage and 100% branch coverage.
- `Main.scala` and `view/Gui.scala` are excluded from coverage in `build.sbt`.
- Coveralls uses the same scoverage report, so Main and GUI are excluded there as well.
- Stryker4s mutates all production code except `Main.scala` and `view/Gui.scala`.
- Stryker4s reaches 100% mutation score.

## Design Patterns

The project currently uses the following design patterns:

- Observer Pattern: `Observable` and `Observer` decouple controller updates from TUI and GUI rendering.
- Factory Method Pattern: `DieFactory` centralizes dice creation.
- Strategy Pattern: `CombinationStrategy` implementations encapsulate scoring combination checks.
- Command Pattern: `GameStateCommand` and `UndoManager` implement undo and redo.
- State Pattern: `PhaseState` implementations encapsulate automatic phase transitions.
- Dependency Injection Pattern: `AppModule` and `AppInjector` build components through interfaces.

## Testing

Important test areas:

```text
ModelSpec
GameControllerTest
PhaseStateTest
UndoManagerTest
ObserverTest
TuiTest
AppModuleTest
```

The tests cover:

- dice evaluation and bonus metadata
- combination detection
- game phase transitions
- command parsing with `Try`
- terminal rendering
- observer registration and notification
- dependency injection wiring
- undo and redo
- invalid commands and edge cases

## Continuous Integration and Coverage

The project uses GitHub Actions as the CI system. On each push or pull request, the project is built and tested automatically.

Code coverage is generated with scoverage and uploaded to Coveralls. The Coveralls badge at the top of this README shows the current coverage status of the main branch.

Main and GUI are excluded from coverage because they are startup and Swing UI code:

```scala
Test / testOptions += Tests.Filter(testName =>
  !testName.endsWith("MainTest") && !testName.endsWith("GuiTest")
)
coverageExcludedFiles := ".*Main.scala;.*Gui.scala"
coverageFailOnMinimum := true
coverageMinimumStmtTotal := 100
coverageMinimumBranchTotal := 100
```

## Mutation Testing

Stryker4s is used to evaluate the strength of the test suite. Mutation testing changes small parts of the production code and checks whether the tests detect those changes.

The current Stryker4s configuration mutates all production code except `Main.scala` and `view/Gui.scala`:

```hocon
mutate = [
  "src/main/scala/controller/*.scala",
  "src/main/scala/model/*.scala",
  "src/main/scala/util/*.scala",
  "src/main/scala/view/IView.scala",
  "src/main/scala/view/Tui.scala",
  "src/main/scala/di/*.scala"
]
```

Run mutation testing with:

```bash
sbt stryker
```

## Project Structure

```text
src/main/scala
|-- Main.scala
|-- controller
|   |-- GameController.scala
|   |-- GameCommandAction.scala
|   |-- IController.scala
|   |-- PhaseState.scala
|   `-- UndoManager.scala
|-- di
|   `-- AppModule.scala
|-- model
|   |-- CombinationStrategy.scala
|   |-- DieFactory.scala
|   `-- Model.scala
|-- util
|   `-- Observer.scala
`-- view
    |-- Gui.scala
    |-- IView.scala
    `-- Tui.scala

src/test/scala
|-- MainTest.scala
|-- controller
|   |-- GameControllerTest.scala
|   |-- PhaseStateTest.scala
|   `-- UndoManagerTest.scala
|-- di
|   `-- AppModuleTest.scala
|-- model
|   `-- ModelTest.scala
|-- util
|   `-- ObserverTest.scala
`-- view
    |-- GuiTest.scala
    `-- TuiTest.scala
```

`MainTest` and `GuiTest` are kept in the repository, but the default sbt test configuration excludes them from the official test and coverage run.
