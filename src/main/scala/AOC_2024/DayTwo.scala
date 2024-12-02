package AOC_2024

import shared.Coord.extractInts
import shared.UpdaterHelpers.removeAtIndex
import shared.{DayChallenge, TestData}

object DayTwo extends DayChallenge[Int, Int] {
  override def partOne(l: List[String]): Int =
    l.map(extractInts).count(isSafe)

  override def partTwo(l: List[String]): Int =
    l.map(extractInts).count(line => isSafe(line) || isSafeRemovingLevel(line))

  private def isSafe(levels: List[Int]): Boolean =
    val diffs = levels.sliding(2).collect { case List(a, b) => b - a }.toList
    diffs.forall(i => i <= -1 && i >= -3) || diffs.forall(i => i >= 1 && i <= 3)

  private def isSafeRemovingLevel(ints: List[Int]): Boolean =
    ints.indices.exists(i => isSafe(removeAtIndex(ints, i)))
}

object DayTwoData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "7 6 4 2 1",
    "1 2 7 8 9",
    "9 7 6 2 1",
    "1 3 2 4 5",
    "8 6 4 4 1",
    "1 3 6 7 9"
  )
  override val expectedPartOne: Option[Int] = Some(2)
  override val expectedPartTwo: Option[Int] = Some(4)
}