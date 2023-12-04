package AOC_2023

import shared.{DayChallenge, Helpers, TestData}

object DayFour extends DayChallenge[Int, Int] with Helpers {
  override def partOne(input: List[String]): Int =
    input.map(getGameScoreAndNumberMatched(_)._2).sum

  override def partTwo(input: List[String]): Int =
    val initialScoresAndCounts = input.map(getGameScoreAndNumberMatched)
    var mapOfCards = input.indices.map(i => i -> 1).toMap
    initialScoresAndCounts.zipWithIndex.foreach { case ((count, _), i) =>
      (i + 1 to i + count).foreach(cardIndex =>
        mapOfCards.get(cardIndex) match
          case Some(currentCount) =>
            mapOfCards = mapOfCards.updated(cardIndex, currentCount + (1 * mapOfCards(i)))
          case None => ()
      )
    }
    mapOfCards.values.sum

  private def getGameScoreAndNumberMatched(s: String) =
    val data = s.split(":")(1).trim
    val (winningNumbers, numbers) = getTwoFromSplit(data, "\\|")
    val count = extractInts(numbers).count(n => extractInts(winningNumbers).contains(n))
    (count, scala.math.pow(2, count - 1).toInt)
}

object DayFourData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
  )
  override val expectedPartOne: Option[Int] = Some(13)
  override val expectedPartTwo: Option[Int] = Some(30)
}