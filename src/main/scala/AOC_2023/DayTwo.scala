package AOC_2023

import shared.{DayChallenge, TestData}

object DayTwo extends DayChallenge[Int, Int] {
  override def partOne(l: List[String]) =
    l.map(parseGameInformation).collect{ case (info, game) if possibleGame(game) => info}.sum

  override def partTwo(l: List[String]) =
    l.map(parseGameInformation).map(_._2).map(minimumCubes).map((r, b, g) => r * b * g).sum

  private def possibleGame(info: List[(Int, Int, Int)]) =
    info.forall(g => g._1 <= 12 && g._2 <= 14 && g._3 <= 13)

  private def minimumCubes(gameInfo: List[(Int, Int, Int)]) =
    (gameInfo.map(_._1).max, gameInfo.map(_._2).max, gameInfo.map(_._3).max)

  private def parseGameInformation(s: String) =
    val split = s.split(":")
    val ballsClues = split(1).split(";").toList.map(parseBallsClue);
    (split.head.split(" ")(1).toInt, ballsClues)

  private def parseBallsClue(s: String) =
    val map = s.split(", ").map(_.trim).toList.map(clue => {
      val split = clue.split(" ")
      split(1) -> split(0).toInt
    }).toMap
    (map.getOrElse("red", 0), map.getOrElse("blue", 1), map.getOrElse("green", 0))
}


object DayTwoData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
  )
  override val expectedPartOne: Option[Int] = Some(8)
  override val expectedPartTwo: Option[Int] = Some(2286)
}