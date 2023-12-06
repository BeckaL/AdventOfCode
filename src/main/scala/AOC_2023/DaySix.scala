package AOC_2023

import shared.{DayChallenge, Helpers, TestData}

object DaySix extends DayChallenge[Int, Long] with Helpers {
  override def partOne(l: List[String]): Int =
    val timesAndDistances = extractInts(l.head).zip(extractInts(l(1)))
    timesAndDistances.map((time, distance) => calculateNumberOfWinningWays(time, distance)).product

  private def calculateNumberOfWinningWays(time: Int, distance: Int) =
    (1 until time).toList.count(i => (time - i) * i > distance)

  override def partTwo(l: List[String]): Long =
    val time = extractInts(l.head).map(_.toString).mkString("").toLong
    val distanceToWin = extractInts(l(1)).map(_.toString).mkString("").toLong + 1
    val max = ((time + Math.sqrt((time * time) - (4 * distanceToWin))) / 2).ceil
    val min = ((time - Math.sqrt((time * time) - (4 * distanceToWin))) / 2).ceil
    (max - min).toLong
}

object DaySixData extends TestData[Int, Long] {
  override val testData: List[String] = List(
    "Time:      7  15   30",
      "Distance:  9  40  200"
  )
  override val expectedPartOne: Option[Int] = Some(288)
  override val expectedPartTwo: Option[Long] = Some(71503L)
}