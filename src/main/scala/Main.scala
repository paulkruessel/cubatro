@main def main(): Unit =
  GameController.run(
    discards = 4,
    maxAvailableDice = 8,
    maxRerolls = 4,
    targetScore = 1000
  )