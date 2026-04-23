import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers._

class GameControllerSpec extends AnyWordSpec {
  private val ansiRegex = "\\u001b\\[[;\\d]*m".r

  private def fixedDie(value: Int, bonusType: BonusType = BonusType.None, bonusValue: Int = 0): Die =
    Die(value, value, bonusType, bonusValue)

  private def rolled(value: Int, die: Die): RolledDie = RolledDie(die, value)

  private def baseState(phase: Phase): GameState =
    GameState(
      bag = Nil,
      availableDice = Nil,
      maxAvailableDice = 5,
      selectedDice = Nil,
      diceInPlay = Nil,
      diceToRoll = Nil,
      lockedRows = Nil,
      cupgrades = Nil,
      discards = 1,
      rerolls = 2,
      totalRerolls = 2,
      plays = 2,
      targetScore = 50,
      score = 0,
      phase = phase
    )

  private final class ScriptedIO(initialInputs: List[String]) {
    private var inputs = initialInputs
    private val out = new StringBuilder

    val io: ControllerIO = ControllerIO(
      readLine = () => {
        val next = inputs.headOption.getOrElse("quit")
        inputs = inputs.drop(1)
        next
      },
      write = text => out.append(text)
    )

    def output: String = out.toString
  }

  private def invokePrivate(name: String, argTypes: Seq[Class[?]], args: Seq[AnyRef]): AnyRef = {
    val method = GameController.getClass.getDeclaredMethod(name, argTypes*)
    method.setAccessible(true)
    method.invoke(GameController, args*)
  }

  "GameController" should {
    "provide sensible default initial state values" in {
      val state = GameController.defaultInitialState()

      state.maxAvailableDice.shouldBe(8)
      state.discards.shouldBe(2)
      state.rerolls.shouldBe(2)
      state.totalRerolls.shouldBe(2)
      state.targetScore.shouldBe(3000)
      state.phase.shouldBe(Phase.Draw)
      state.bag.nonEmpty.shouldBe(true)
    }

    "apply run start parameters and stop on quit" in {
      val scripted = new ScriptedIO(List("quit"))
      val start = baseState(Phase.Select)

      val end = GameController.run(
        initialState = start,
        io = scripted.io,
        maxAvailableDice = 7,
        discards = 3,
        maxRerolls = 4,
        targetScore = 200
      )

      end.maxAvailableDice.shouldBe(7)
      end.discards.shouldBe(3)
      end.rerolls.shouldBe(4)
      end.totalRerolls.shouldBe(4)
      end.targetScore.shouldBe(200)
      scripted.output.should(include("Game stopped by player"))
    }

    "handle win and lose terminal phases" in {
      val winIO = new ScriptedIO(Nil)
      val loseIO = new ScriptedIO(Nil)

      val winEnd = GameController.run(initialState = baseState(Phase.Win), io = winIO.io)
      val loseEnd = GameController.run(initialState = baseState(Phase.Lose), io = loseIO.io)

      winEnd.phase.shouldBe(Phase.Win)
      loseEnd.phase.shouldBe(Phase.Lose)
      winIO.output.should(include("You win"))
      loseIO.output.should(include("You lose"))
    }

    "show help and error messages for invalid select actions" in {
      val scripted = new ScriptedIO(List("help", "select", "discard", "reroll", "oops", "quit"))
      val start = baseState(Phase.Select).copy(discards = 0)

      GameController.run(initialState = start, io = scripted.io)

      scripted.output.should(include("Select phase"))
      scripted.output.should(include("Action error"))
      scripted.output.should(include("Input error"))
    }

    "reroll picked dice and continue until score" in {
      val d1 = fixedDie(6)
      val d2 = fixedDie(1)
      val start = baseState(Phase.PickOut).copy(
        rerolls = 1,
        totalRerolls = 3,
        diceInPlay = List(rolled(2, d1), rolled(5, d2)),
        plays = 1,
        targetScore = 99999
      )
      val scripted = new ScriptedIO(List("pick 0 1", "reroll", "score", "score", "quit"))

      val end = GameController.run(initialState = start, io = scripted.io, maxRerolls = 3)

      end.lockedRows.nonEmpty.shouldBe(true)
      end.rerolls.shouldBe(3)
      scripted.output.should(include("PickOut phase"))
      scripted.output.should(include("Score phase"))
    }

    "render a fixed-width board with colored dice" in {
      val state = baseState(Phase.Select).copy(
        availableDice = List(
          fixedDie(1, BonusType.None, 0),
          fixedDie(2, BonusType.Chips, 2),
          fixedDie(3, BonusType.Mult, 1)
        ),
        diceInPlay = List(
          rolled(2, fixedDie(2, BonusType.Chips, 2)),
          rolled(3, fixedDie(3, BonusType.Mult, 1))
        )
      )

      val board = GameController.renderBoard(state)
      board.should(include("CUBATRO"))
      board.should(include("\u001b[34m"))
      board.should(include("\u001b[31m"))

      val visibleLines = board.split("\\n").toList.filter(_.nonEmpty).map(line => ansiRegex.replaceAllIn(line, ""))
      visibleLines.foreach(line => line.length.should(be <= 72))
    }

    "cover private helper branches" in {
      Phase.values.foreach { phase =>
        val helpText = invokePrivate("helpText", Seq(classOf[Phase]), Seq(phase.asInstanceOf[AnyRef])).asInstanceOf[String]
        helpText.nonEmpty.shouldBe(true)
      }

      val drawPrompt = invokePrivate("phasePrompt", Seq(classOf[Phase]), Seq(Phase.Draw.asInstanceOf[AnyRef])).asInstanceOf[String]
      drawPrompt.should(include("Phase Draw"))

      val header = invokePrivate("helpHeader", Seq(classOf[Phase]), Seq(Phase.Score.asInstanceOf[AnyRef])).asInstanceOf[String]
      header.should(include("Score phase"))

      val die = fixedDie(4, BonusType.Mult, 2)
      val rolledDieText = invokePrivate(
        "renderDie",
        Seq(classOf[Die], java.lang.Boolean.TYPE),
        Seq(die.asInstanceOf[AnyRef], java.lang.Boolean.TRUE)
      ).asInstanceOf[String]
      rolledDieText.should(include("[?"))

      val parsed = invokePrivate("parseCommand", Seq(classOf[String]), Seq("select 1 a".asInstanceOf[AnyRef]))
        .asInstanceOf[Either[String, GameCommand]]
      parsed.isLeft.shouldBe(true)
    }

    "cover command aliases, unknown and empty command parsing" in {
      val h = invokePrivate("parseCommand", Seq(classOf[String]), Seq("h".asInstanceOf[AnyRef]))
        .asInstanceOf[Either[String, GameCommand]]
      val q = invokePrivate("parseCommand", Seq(classOf[String]), Seq("exit".asInstanceOf[AnyRef]))
        .asInstanceOf[Either[String, GameCommand]]
      val d = invokePrivate("parseCommand", Seq(classOf[String]), Seq("d".asInstanceOf[AnyRef]))
        .asInstanceOf[Either[String, GameCommand]]
      val p = invokePrivate("parseCommand", Seq(classOf[String]), Seq("p".asInstanceOf[AnyRef]))
        .asInstanceOf[Either[String, GameCommand]]
      val r = invokePrivate("parseCommand", Seq(classOf[String]), Seq("r".asInstanceOf[AnyRef]))
        .asInstanceOf[Either[String, GameCommand]]
      val s = invokePrivate("parseCommand", Seq(classOf[String]), Seq("lock".asInstanceOf[AnyRef]))
        .asInstanceOf[Either[String, GameCommand]]
      val pick = invokePrivate("parseCommand", Seq(classOf[String]), Seq("pick 1 2".asInstanceOf[AnyRef]))
        .asInstanceOf[Either[String, GameCommand]]
      val select = invokePrivate("parseCommand", Seq(classOf[String]), Seq("select 0 3".asInstanceOf[AnyRef]))
        .asInstanceOf[Either[String, GameCommand]]
      val empty = invokePrivate("parseCommand", Seq(classOf[String]), Seq("   ".asInstanceOf[AnyRef]))
        .asInstanceOf[Either[String, GameCommand]]
      val unknown = invokePrivate("parseCommand", Seq(classOf[String]), Seq("boom".asInstanceOf[AnyRef]))
        .asInstanceOf[Either[String, GameCommand]]

      h.shouldBe(Right(GameCommand.Help))
      q.shouldBe(Right(GameCommand.Quit))
      d.shouldBe(Right(GameCommand.Discard))
      p.shouldBe(Right(GameCommand.PlaySelected))
      r.shouldBe(Right(GameCommand.Reroll))
      s.shouldBe(Right(GameCommand.ScoreCurrent))
      pick.shouldBe(Right(GameCommand.Pick(List(1, 2))))
      select.shouldBe(Right(GameCommand.Select(List(0, 3))))
      empty.isLeft.shouldBe(true)
      unknown.isLeft.shouldBe(true)
    }

    "cover end evaluation transitions and disallowed phase action" in {
      val disallowed = invokePrivate(
        "applyCommand",
        Seq(classOf[GameState], classOf[GameCommand]),
        Seq(baseState(Phase.Draw).asInstanceOf[AnyRef], GameCommand.Help.asInstanceOf[AnyRef])
      ).asInstanceOf[Either[String, GameState]]
      disallowed.isLeft.shouldBe(true)

      val winIO = new ScriptedIO(Nil)
      val loseIO = new ScriptedIO(Nil)
      val drawIO = new ScriptedIO(List("quit"))

      val win = GameController.run(
        initialState = baseState(Phase.EndEval).copy(score = 200, targetScore = 50),
        io = winIO.io,
        targetScore = 50
      )
      val lose = GameController.run(
        initialState = baseState(Phase.EndEval).copy(score = 10, targetScore = 50, plays = 0),
        io = loseIO.io,
        targetScore = 50
      )
      val draw = GameController.run(
        initialState = baseState(Phase.EndEval).copy(score = 10, targetScore = 50, plays = 1),
        io = drawIO.io,
        targetScore = 50
      )

      win.phase.shouldBe(Phase.Win)
      lose.phase.shouldBe(Phase.Lose)
      draw.phase.shouldBe(Phase.Select)
    }

    "cover render edge cases for helper branches" in {
      val oddBoard = GameController.renderBoard(
        baseState(Phase.Select).copy(
          diceInPlay = List(rolled(9, fixedDie(9, BonusType.None, 0))),
          lockedRows = List(LockedRow(Nil, Combination.Ones, 1))
        )
      )

      oddBoard.should(include("Best combo now: none"))

      val wrapped = invokePrivate("wrapText", Seq(classOf[String]), Seq("   ".asInstanceOf[AnyRef]))
        .asInstanceOf[List[String]]
      wrapped.shouldBe(List(""))

      val contentLine = invokePrivate(
        "renderContentLine",
        Seq(classOf[String]),
        Seq(("X" * 200).asInstanceOf[AnyRef])
      ).asInstanceOf[String]
      ansiRegex.replaceAllIn(contentLine, "").length.shouldBe(202)
    }


    "cover select phase command handler branches directly" in {
      val d1 = fixedDie(1)
      val d2 = fixedDie(2)
      val selectState = baseState(Phase.Select).copy(availableDice = List(d1, d2))

      val emptySelect = invokePrivate(
        "applySelectPhaseCommand",
        Seq(classOf[GameState], classOf[GameCommand]),
        Seq(selectState.asInstanceOf[AnyRef], GameCommand.Select(Nil).asInstanceOf[AnyRef])
      ).asInstanceOf[Either[String, GameState]]
      emptySelect.shouldBe(Left("select needs one or more indices."))

      val alreadyFull = invokePrivate(
        "applySelectPhaseCommand",
        Seq(classOf[GameState], classOf[GameCommand]),
        Seq(selectState.copy(selectedDice = List.fill(5)(d1)).asInstanceOf[AnyRef], GameCommand.Select(List(0)).asInstanceOf[AnyRef])
      ).asInstanceOf[Either[String, GameState]]
      alreadyFull.shouldBe(Left("you can select at most 5 dice."))

      val selected = invokePrivate(
        "applySelectPhaseCommand",
        Seq(classOf[GameState], classOf[GameCommand]),
        Seq(selectState.asInstanceOf[AnyRef], GameCommand.Select(List(0)).asInstanceOf[AnyRef])
      ).asInstanceOf[Either[String, GameState]]
      selected.toOption.getOrElse(fail("expected Right")).selectedDice.shouldBe(List(d1))

      val noDiscards = invokePrivate(
        "applySelectPhaseCommand",
        Seq(classOf[GameState], classOf[GameCommand]),
        Seq(selectState.copy(selectedDice = List(d1), discards = 0).asInstanceOf[AnyRef], GameCommand.Discard.asInstanceOf[AnyRef])
      ).asInstanceOf[Either[String, GameState]]
      noDiscards.shouldBe(Left("no discards left."))

      val noSelectedDiscard = invokePrivate(
        "applySelectPhaseCommand",
        Seq(classOf[GameState], classOf[GameCommand]),
        Seq(selectState.asInstanceOf[AnyRef], GameCommand.Discard.asInstanceOf[AnyRef])
      ).asInstanceOf[Either[String, GameState]]
      noSelectedDiscard.shouldBe(Left("no selected dice to discard."))

      val discarded = invokePrivate(
        "applySelectPhaseCommand",
        Seq(classOf[GameState], classOf[GameCommand]),
        Seq(selectState.copy(selectedDice = List(d1), discards = 1).asInstanceOf[AnyRef], GameCommand.Discard.asInstanceOf[AnyRef])
      ).asInstanceOf[Either[String, GameState]]
      discarded.toOption.getOrElse(fail("expected Right")).selectedDice.shouldBe(Nil)
      discarded.toOption.getOrElse(fail("expected Right")).discards.shouldBe(0)

      val playWithoutSelection = invokePrivate(
        "applySelectPhaseCommand",
        Seq(classOf[GameState], classOf[GameCommand]),
        Seq(selectState.asInstanceOf[AnyRef], GameCommand.PlaySelected.asInstanceOf[AnyRef])
      ).asInstanceOf[Either[String, GameState]]
      playWithoutSelection.shouldBe(Left("select at least one die before playing."))

      val played = invokePrivate(
        "applySelectPhaseCommand",
        Seq(classOf[GameState], classOf[GameCommand]),
        Seq(selectState.copy(selectedDice = List(d1)).asInstanceOf[AnyRef], GameCommand.PlaySelected.asInstanceOf[AnyRef])
      ).asInstanceOf[Either[String, GameState]]
      played.toOption.getOrElse(fail("expected Right")).phase.shouldBe(Phase.Roll)
      played.toOption.getOrElse(fail("expected Right")).selectedDice.shouldBe(Nil)

      val unsupported = invokePrivate(
        "applySelectPhaseCommand",
        Seq(classOf[GameState], classOf[GameCommand]),
        Seq(selectState.asInstanceOf[AnyRef], GameCommand.Help.asInstanceOf[AnyRef])
      ).asInstanceOf[Either[String, GameState]]
      unsupported.shouldBe(Left("allowed in Select: select, discard, play, help, quit."))
    }

    "cover pick out and score phase command handler branches directly" in {
      val d1 = fixedDie(1)
      val d2 = fixedDie(2)
      val r1 = rolled(1, d1)
      val r2 = rolled(2, d2)
      val pickState = baseState(Phase.PickOut).copy(diceInPlay = List(r1, r2), diceToRoll = Nil, rerolls = 1)

      val emptyPick = invokePrivate(
        "applyPickOutPhaseCommand",
        Seq(classOf[GameState], classOf[GameCommand]),
        Seq(pickState.asInstanceOf[AnyRef], GameCommand.Pick(Nil).asInstanceOf[AnyRef])
      ).asInstanceOf[Either[String, GameState]]
      emptyPick.shouldBe(Left("pick needs one or more indices."))

      val picked = invokePrivate(
        "applyPickOutPhaseCommand",
        Seq(classOf[GameState], classOf[GameCommand]),
        Seq(pickState.asInstanceOf[AnyRef], GameCommand.Pick(List(1)).asInstanceOf[AnyRef])
      ).asInstanceOf[Either[String, GameState]]
      picked.toOption.getOrElse(fail("expected Right")).diceInPlay.shouldBe(List(r1))
      picked.toOption.getOrElse(fail("expected Right")).diceToRoll.shouldBe(List(r2))

      val noRerolls = invokePrivate(
        "applyPickOutPhaseCommand",
        Seq(classOf[GameState], classOf[GameCommand]),
        Seq(pickState.copy(rerolls = 0, diceToRoll = List(r1)).asInstanceOf[AnyRef], GameCommand.Reroll.asInstanceOf[AnyRef])
      ).asInstanceOf[Either[String, GameState]]
      noRerolls.shouldBe(Left("no rerolls left."))

      val noDiceToReroll = invokePrivate(
        "applyPickOutPhaseCommand",
        Seq(classOf[GameState], classOf[GameCommand]),
        Seq(pickState.asInstanceOf[AnyRef], GameCommand.Reroll.asInstanceOf[AnyRef])
      ).asInstanceOf[Either[String, GameState]]
      noDiceToReroll.shouldBe(Left("pick dice first before reroll."))

      val reroll = invokePrivate(
        "applyPickOutPhaseCommand",
        Seq(classOf[GameState], classOf[GameCommand]),
        Seq(pickState.copy(diceToRoll = List(r1)).asInstanceOf[AnyRef], GameCommand.Reroll.asInstanceOf[AnyRef])
      ).asInstanceOf[Either[String, GameState]]
      reroll.toOption.getOrElse(fail("expected Right")).phase.shouldBe(Phase.Roll)

      val noDiceToScore = invokePrivate(
        "applyPickOutPhaseCommand",
        Seq(classOf[GameState], classOf[GameCommand]),
        Seq(pickState.copy(diceInPlay = Nil).asInstanceOf[AnyRef], GameCommand.ScoreCurrent.asInstanceOf[AnyRef])
      ).asInstanceOf[Either[String, GameState]]
      noDiceToScore.shouldBe(Left("no dice in play to score."))

      val scoreFromPickOut = invokePrivate(
        "applyPickOutPhaseCommand",
        Seq(classOf[GameState], classOf[GameCommand]),
        Seq(pickState.asInstanceOf[AnyRef], GameCommand.ScoreCurrent.asInstanceOf[AnyRef])
      ).asInstanceOf[Either[String, GameState]]
      scoreFromPickOut.toOption.getOrElse(fail("expected Right")).phase.shouldBe(Phase.Score)

      val unsupportedPickOut = invokePrivate(
        "applyPickOutPhaseCommand",
        Seq(classOf[GameState], classOf[GameCommand]),
        Seq(pickState.asInstanceOf[AnyRef], GameCommand.Help.asInstanceOf[AnyRef])
      ).asInstanceOf[Either[String, GameState]]
      unsupportedPickOut.shouldBe(Left("allowed in PickOut: pick, reroll, score, help, quit."))

      val scoreState = baseState(Phase.Score).copy(diceInPlay = List.fill(5)(r1), plays = 2)
      val emptyScore = invokePrivate(
        "applyScorePhaseCommand",
        Seq(classOf[GameState], classOf[GameCommand]),
        Seq(scoreState.copy(diceInPlay = Nil).asInstanceOf[AnyRef], GameCommand.ScoreCurrent.asInstanceOf[AnyRef])
      ).asInstanceOf[Either[String, GameState]]
      emptyScore.shouldBe(Left("no dice in play to score."))

      val scored = invokePrivate(
        "applyScorePhaseCommand",
        Seq(classOf[GameState], classOf[GameCommand]),
        Seq(scoreState.asInstanceOf[AnyRef], GameCommand.ScoreCurrent.asInstanceOf[AnyRef])
      ).asInstanceOf[Either[String, GameState]]
      scored.toOption.getOrElse(fail("expected Right")).phase.shouldBe(Phase.EndEval)
      scored.toOption.getOrElse(fail("expected Right")).plays.shouldBe(1)
      scored.toOption.getOrElse(fail("expected Right")).lockedRows.head.combination.shouldBe(Combination.Yahtzee)

      val scoredAtZeroPlays = invokePrivate(
        "applyScorePhaseCommand",
        Seq(classOf[GameState], classOf[GameCommand]),
        Seq(scoreState.copy(plays = 0).asInstanceOf[AnyRef], GameCommand.ScoreCurrent.asInstanceOf[AnyRef])
      ).asInstanceOf[Either[String, GameState]]
      scoredAtZeroPlays.toOption.getOrElse(fail("expected Right")).plays.shouldBe(0)

      val unsupportedScore = invokePrivate(
        "applyScorePhaseCommand",
        Seq(classOf[GameState], classOf[GameCommand]),
        Seq(scoreState.asInstanceOf[AnyRef], GameCommand.Help.asInstanceOf[AnyRef])
      ).asInstanceOf[Either[String, GameState]]
      unsupportedScore.shouldBe(Left("allowed in Score: score, help, quit."))
    }

    "cover automatic phase and rendering helper edge branches directly" in {
      val d1 = fixedDie(1)
      val rollToScore = invokePrivate(
        "advanceAutomaticPhases",
        Seq(classOf[GameState]),
        Seq(baseState(Phase.Roll).copy(diceToRoll = List(rolled(1, d1)), rerolls = 0).asInstanceOf[AnyRef])
      ).asInstanceOf[GameState]
      rollToScore.phase.shouldBe(Phase.Score)

      val shortFit = invokePrivate("fitPlain", Seq(classOf[String], java.lang.Integer.TYPE), Seq("abc".asInstanceOf[AnyRef], Int.box(5)))
        .asInstanceOf[String]
      val longFit = invokePrivate("fitPlain", Seq(classOf[String], java.lang.Integer.TYPE), Seq("abcdef".asInstanceOf[AnyRef], Int.box(3)))
        .asInstanceOf[String]
      shortFit.shouldBe("abc")
      longFit.shouldBe("abc")

      val plainRolledDie = invokePrivate(
        "renderDie",
        Seq(classOf[Die], java.lang.Boolean.TYPE),
        Seq(d1.asInstanceOf[AnyRef], java.lang.Boolean.TRUE)
      ).asInstanceOf[String]
      plainRolledDie.shouldBe("[?]")

      val emptySection = invokePrivate(
        "sectionBlock",
        Seq(classOf[String], classOf[List[String]]),
        Seq("Title".asInstanceOf[AnyRef], Nil.asInstanceOf[AnyRef])
      ).asInstanceOf[List[String]]
      emptySection.mkString("\n").should(include("-"))

      val emptyTokens = invokePrivate("wrapTokens", Seq(classOf[List[String]]), Seq(Nil.asInstanceOf[AnyRef]))
        .asInstanceOf[List[String]]
      emptyTokens.shouldBe(Nil)

      val wrappedTokens = invokePrivate(
        "wrapTokens",
        Seq(classOf[List[String]]),
        Seq(List("a" * 80, "b" * 80).asInstanceOf[AnyRef])
      ).asInstanceOf[List[String]]
      wrappedTokens.shouldBe(List("a" * 80, "b" * 80))

      val emptyWrappedText = invokePrivate("wrapText", Seq(classOf[String]), Seq("".asInstanceOf[AnyRef]))
        .asInstanceOf[List[String]]
      emptyWrappedText.shouldBe(List(""))

      val longWrappedText = invokePrivate(
        "wrapText",
        Seq(classOf[String]),
        Seq(("a" * 80 + " " + "b" * 80).asInstanceOf[AnyRef])
      ).asInstanceOf[List[String]]
      longWrappedText.shouldBe(List("a" * 80, "b" * 80))
    }

  }
}
