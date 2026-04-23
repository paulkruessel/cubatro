import scala.io.StdIn
import scala.util.Random
import scala.util.Try

enum GameCommand:
	case Help
	case Quit
	case Select(indices: List[Int])
	case Discard
	case PlaySelected
	case Pick(indices: List[Int])
	case Reroll
	case ScoreCurrent

case class ControllerIO(
		readLine: () => String,
		write: String => Unit
)

object ControllerIO:
	val terminal: ControllerIO = ControllerIO(
		readLine = () => StdIn.readLine(),
		write = (text: String) => print(text)
	)

object GameController:
	private val ansiReset = "\u001b[0m"
	private val ansiBlue = "\u001b[34m"
	private val ansiRed = "\u001b[31m"
	private val ansiClear = "\u001b[2J\u001b[H"
	private val boardWidth = 72
	private val innerWidth = boardWidth - 2

	def defaultInitialState(): GameState =
		val plain = Die(bonusType = BonusType.None, bonusValue = 0)
		val chips = Die(bonusType = BonusType.Chips, bonusValue = 2)
		val mult = Die(bonusType = BonusType.Mult, bonusValue = 1)

		GameState(
			bag = Random.shuffle(List.fill(14)(plain) ++ List.fill(6)(chips) ++ List.fill(4)(mult)),
			availableDice = Nil,
			maxAvailableDice = 8,
			selectedDice = Nil,
			diceInPlay = Nil,
			diceToRoll = Nil,
			lockedRows = Nil,
			cupgrades = Nil,
			discards = 2,
			rerolls = 2,
			totalRerolls = 2,
			plays = 6,
			targetScore = 3000,
			score = 0,
			phase = Phase.Draw
		)

	def run(
		initialState: GameState = defaultInitialState(),
		io: ControllerIO = ControllerIO.terminal,
		maxAvailableDice: Int = 8,
		discards: Int = 2,
		maxRerolls: Int = 2,
		targetScore: Int = 3000
	): GameState =
		var state = initialState.copy(
			maxAvailableDice = maxAvailableDice,
			discards = discards,
			rerolls = maxRerolls,
			totalRerolls = maxRerolls,
			targetScore = targetScore
		)
		var running = true

		while running do
			state = advanceAutomaticPhases(state)
			io.write(ansiClear)
			io.write(renderBoard(state))

			state.phase match
				case Phase.Win =>
					io.write("\nYou win. Final score reached!\n")
					running = false
				case Phase.Lose =>
					io.write("\nYou lose. No plays left to reach target score.\n")
					running = false
				case _ =>
					io.write(s"\n${phasePrompt(state.phase)}\n> ")
					val input = Option(io.readLine()).getOrElse("quit")

					parseCommand(input) match
						case Left(error) =>
							io.write(s"\nInput error: $error\n")
							io.write(helpText(state.phase))
						case Right(GameCommand.Help) =>
							io.write(helpText(state.phase))
						case Right(GameCommand.Quit) =>
							io.write("\nGame stopped by player.\n")
							running = false
						case Right(command) =>
							applyCommand(state, command) match
								case Left(error) =>
									io.write(s"\nAction error: $error\n")
								case Right(nextState) =>
									state = nextState

		state

	private def advanceAutomaticPhases(initial: GameState): GameState =
		var state = initial
		var continue = true

		while continue do
			state.phase match
				case Phase.Draw =>
					state = state.drawDice().copy(phase = Phase.Select)
				case Phase.Roll =>
					val rolledState = state.rollDice()
					val nextPhase = if rolledState.rerolls <= 0 then Phase.Score else Phase.PickOut
					state = rolledState.copy(phase = nextPhase)
				case Phase.EndEval =>
					state = evaluateRoundEnd(state)
				case _ =>
					continue = false

		state

	private def evaluateRoundEnd(state: GameState): GameState =
		if state.score >= state.targetScore then state.copy(phase = Phase.Win)
		else if state.plays <= 0 then state.copy(phase = Phase.Lose)
		else state.copy(phase = Phase.Draw)

	private def applyCommand(state: GameState, command: GameCommand): Either[String, GameState] =
		state.phase match
			case Phase.Select => applySelectPhaseCommand(state, command)
			case Phase.PickOut => applyPickOutPhaseCommand(state, command)
			case Phase.Score => applyScorePhaseCommand(state, command)
			case Phase.Draw | Phase.Roll | Phase.EndEval | Phase.Win | Phase.Lose =>
				Left(s"No player actions accepted in phase ${state.phase}.")

	private def applySelectPhaseCommand(state: GameState, command: GameCommand): Either[String, GameState] =
		command match
			case GameCommand.Select(indices) =>
				if indices.isEmpty then Left("select needs one or more indices.")
				else if state.selectedDice.length >= 5 then Left("you can select at most 5 dice.")
				else Right(state.selectDice(indices))
			case GameCommand.Discard =>
				if state.discards <= 0 then Left("no discards left.")
				else if state.selectedDice.isEmpty then Left("no selected dice to discard.")
				else Right(state.discardDice())
			case GameCommand.PlaySelected =>
				if state.selectedDice.isEmpty then Left("select at least one die before playing.")
				else Right(state.addDiceToPlay().copy(phase = Phase.Roll))
			case _ =>
				Left("allowed in Select: select, discard, play, help, quit.")

	private def applyPickOutPhaseCommand(state: GameState, command: GameCommand): Either[String, GameState] =
		command match
			case GameCommand.Pick(indices) =>
				if indices.isEmpty then Left("pick needs one or more indices.")
				else Right(state.selectPlayedDice(indices))
			case GameCommand.Reroll =>
				if state.rerolls <= 0 then Left("no rerolls left.")
				else if state.diceToRoll.isEmpty then Left("pick dice first before reroll.")
				else Right(state.copy(phase = Phase.Roll))
			case GameCommand.ScoreCurrent =>
				if state.diceInPlay.isEmpty then Left("no dice in play to score.")
				else Right(state.copy(phase = Phase.Score))
			case _ =>
				Left("allowed in PickOut: pick, reroll, score, help, quit.")

	private def applyScorePhaseCommand(state: GameState, command: GameCommand): Either[String, GameState] =
		command match
			case GameCommand.ScoreCurrent =>
				if state.diceInPlay.isEmpty then Left("no dice in play to score.")
				else
					val scored = state.scoreDiceInPlay()
					Right(scored.copy(plays = math.max(0, scored.plays - 1), phase = Phase.EndEval))
			case _ =>
				Left("allowed in Score: score, help, quit.")

	private def parseCommand(input: String): Either[String, GameCommand] =
		val normalized = input.trim.toLowerCase
		if normalized.isEmpty then Left("empty input")
		else
			val tokens = normalized.replace(",", " ").split("\\s+").toList.filter(_.nonEmpty)
			tokens match
				case "help" :: Nil | "h" :: Nil => Right(GameCommand.Help)
				case "quit" :: Nil | "q" :: Nil | "exit" :: Nil => Right(GameCommand.Quit)
				case "discard" :: Nil | "d" :: Nil => Right(GameCommand.Discard)
				case "play" :: Nil | "p" :: Nil => Right(GameCommand.PlaySelected)
				case "reroll" :: Nil | "r" :: Nil => Right(GameCommand.Reroll)
				case "score" :: Nil | "s" :: Nil | "lock" :: Nil => Right(GameCommand.ScoreCurrent)
				case "select" :: tail => parseIndices(tail).map(GameCommand.Select.apply)
				case "pick" :: tail => parseIndices(tail).map(GameCommand.Pick.apply)
				case _ => Left(s"unknown command: ${tokens.head}")

	private def parseIndices(tokens: List[String]): Either[String, List[Int]] =
		val parsed = tokens.map(token => Try(token.toInt).toOption)
		if parsed.contains(None) then Left("indices must be integer values")
		else Right(parsed.flatten)

	private def phasePrompt(phase: Phase): String =
		phase match
			case Phase.Select => "Select phase: select <indices> | discard | play | help | quit"
			case Phase.PickOut => "PickOut phase: pick <indices> | reroll | score | help | quit"
			case Phase.Score => "Score phase: score | help | quit"
			case _ => s"Phase $phase"

	private def helpText(phase: Phase): String =
		phase match
			case Phase.Draw =>
				"""
				Draw phase:
				- The game fills your hand up to the current maximum.
				- No input is needed here.
				- Goal: get ready to select dice for the next play.
				""".trim
			case Phase.Select =>
				"""
				Select phase:
				- Choose dice from your hand with: select <indices>
				- You can discard selected dice with: discard
				- Start the play with: play
				- Goal: build a useful set of at most 5 dice for the roll.
				""".trim
			case Phase.Roll =>
				"""
				Roll phase:
				- The selected dice are rolled automatically.
				- No input is needed here.
				- Goal: get the rolled dice into play.
				""".trim
			case Phase.PickOut =>
				"""
				PickOut phase:
				- Move dice from In Play to To Roll with: pick <indices>
				- Reroll the chosen dice with: reroll
				- Finish the row and score with: score
				- Goal: improve the current rolled set or lock in a score.
				""".trim
			case Phase.Score =>
				"""
				Score phase:
				- Finalize the current dice row with: score
				- No more rerolls are possible in this phase.
				- Goal: lock the best combination and gain points.
				""".trim
			case Phase.EndEval =>
				"""
				EndEval phase:
				- The game checks whether you reached the target score.
				- If not, the next round starts automatically.
				- Goal: determine whether you win, lose, or continue.
				""".trim
			case Phase.Win =>
				"""
				Win phase:
				- You reached the target score.
				- The game is finished.
				""".trim
			case Phase.Lose =>
				"""
				Lose phase:
				- You can no longer continue.
				- The game is finished.
				""".trim

	private def helpHeader(phase: Phase): String =
		s"\n${phasePrompt(phase)}\n\n"

	def renderBoard(state: GameState): String =
		val lines = List.newBuilder[String]
		lines += borderLine()
		lines += centeredLine("CUBATRO")
		lines += borderLine()
		lines ++= renderWrappedStatLine(
			s"Target: ${state.targetScore}  Score: ${state.score}  Phase: ${state.phase}"
		)
		lines ++= renderWrappedStatLine(
			s"Plays: ${state.plays}  Rerolls: ${state.rerolls}  Discards: ${state.discards}"
		)
		lines += borderLine()
		lines ++= sectionBlock(s"Hand (${state.availableDice.length}):", renderDice(state.availableDice, isRolled = false))
		lines ++= sectionBlock(s"Selected (${state.selectedDice.length}):", renderDice(state.selectedDice, isRolled = false))
		lines ++= sectionBlock(s"In Play (${state.diceInPlay.length}):", renderRolledDice(state.diceInPlay))
		lines ++= sectionBlock(s"To Roll (${state.diceToRoll.length}):", renderRolledDice(state.diceToRoll))
		lines += borderLine()
		lines ++= renderWrappedStatLine(s"Best combo now: ${currentBestCombination(state)}")
		lines ++= sectionBlock("Locked rows:", renderLockedRows(state.lockedRows))
		lines += borderLine()
		lines.result().mkString("\n") + "\n"

	private def renderDice(dice: List[Die], isRolled: Boolean): List[String] =
		if dice.isEmpty then List("-")
		else
			wrapTokens(dice.zipWithIndex.map((die, idx) => s"$idx:${renderDie(die, isRolled)}"))

	private def renderRolledDice(dice: List[RolledDie]): List[String] =
		if dice.isEmpty then List("-")
		else
			wrapTokens(dice.zipWithIndex.map((die, idx) => s"$idx:${renderRolled(die)}"))

	private def renderDie(die: Die, isRolled: Boolean): String =
		val color = die.bonusType match
			case BonusType.Chips => ansiBlue
			case BonusType.Mult => ansiRed
			case BonusType.None => ""
		val bonus = die.bonusType match
			case BonusType.None => ""
			case BonusType.Chips => s"+${die.bonusValue}C"
			case BonusType.Mult => s"+${die.bonusValue}M"
		val text =
			if isRolled then s"[?${if bonus.nonEmpty then ":" + bonus else ""}]"
			else s"[d${die.min}-${die.max}${if bonus.nonEmpty then ":" + bonus else ""}]"

		if color.isEmpty then text else s"$color$text$ansiReset"

	private def renderRolled(die: RolledDie): String =
		val color = die.die.bonusType match
			case BonusType.Chips => ansiBlue
			case BonusType.Mult => ansiRed
			case BonusType.None => ""
		val bonus = die.die.bonusType match
			case BonusType.None => ""
			case BonusType.Chips => s"+${die.die.bonusValue}C"
			case BonusType.Mult => s"+${die.die.bonusValue}M"
		val text = s"[${die.value}${if bonus.nonEmpty then ":" + bonus else ""}]"
		if color.isEmpty then text else s"$color$text$ansiReset"

	private def currentBestCombination(state: GameState): String =
		if state.diceInPlay.isEmpty then "-"
		else
			val combos = matchingCombinations(state.diceInPlay)
			if combos.isEmpty then "none"
			else combos.maxBy(_.rank).toString

	private def renderLockedRows(rows: List[LockedRow]): List[String] =
		if rows.isEmpty then List("-")
		else
			rows.zipWithIndex.flatMap { (row, idx) =>
				wrapText(s"${idx + 1}. ${row.combination} -> ${row.score}")
			}

	private def borderLine(): String =
		"+" + "-" * innerWidth + "+"

	private def fitPlain(text: String, maxWidth: Int): String =
		if text.length <= maxWidth then text
		else text.take(maxWidth)

	private def centeredLine(text: String): String =
		val clipped = fitPlain(text, innerWidth)
		val leftPadding = (innerWidth - clipped.length) / 2
		val rightPadding = innerWidth - clipped.length - leftPadding
		"|" + " " * leftPadding + clipped + " " * rightPadding + "|"

	private def renderWrappedStatLine(text: String): List[String] =
		wrapText(text).map(renderContentLine)

	private def sectionBlock(title: String, bodyLines: List[String]): List[String] =
		val titleLines = wrapText(title)
		val body = if bodyLines.isEmpty then List("-") else bodyLines
		titleLines.map(renderContentLine) ++ body.map(renderContentLine)

	private def renderContentLine(text: String): String =
		val visible = visibleLength(text)
		val padded = if visible >= innerWidth then text else text + " " * (innerWidth - visible)
		"|" + padded + "|"

	private def wrapTokens(tokens: List[String]): List[String] =
		if tokens.isEmpty then List.empty
		else
			val lines = scala.collection.mutable.ListBuffer.empty[String]
			var current = ""

			for token <- tokens do
				val separator = if current.isEmpty then "" else " "
				val candidate = current + separator + token
				if visibleLength(candidate) <= innerWidth then
					current = candidate
				else
					if current.nonEmpty then lines += current
					current = token
			lines.toList :+ current

	private def wrapText(text: String): List[String] =
		if text.isEmpty then List("")
		else
			val words = text.split("\\s+").toList.filter(_.nonEmpty)
			if words.isEmpty then List("")
			else
				val lines = scala.collection.mutable.ListBuffer.empty[String]
				var current = ""
				for word <- words do
					val candidate = if current.isEmpty then word else s"$current $word"
					if candidate.length <= innerWidth then
						current = candidate
					else
						if current.nonEmpty then lines += current
						current = word
				lines.toList :+ current

	private def visibleLength(text: String): Int =
		text.replaceAll("\\u001b\\[[;\\d]*m", "").length

