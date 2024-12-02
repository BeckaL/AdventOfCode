package AOC_2024

import shared.{DayChallenge, TestData}

object DayTwo extends DayChallenge[Int, Int]{
  override def partOne(l: List[String]): Int =
    l.map(getDiffs).count(isSafe)

  private def isSafe(diffs: List[Int]): Boolean =
    diffs.forall(i => i <= -1 && i >= -3) || diffs.forall(i => i >= 1 && i <= 3)

  override def partTwo(l: List[String]): Int = {
    l.map(getDiffs).count(isSafeRemovingLevel)
  }

  private def getDiffs(line: String): List[Int] =
    line.split(" ").map(_.toInt).sliding(2).map(_.toList).map { case List(a, b) => b - a }.toList

  private def isSafeRemovingLevel(diffs: List[Int]): Boolean =
    if (isSafe(diffs)) {
      return true
    }
    val outliersWithIndices = diffs.zipWithIndex.filter{ case (diff, _) => diff == 0 || diff < -3 || diff > 3}
    if (outliersWithIndices.size == 1) {
      val (index, value) = outliersWithIndices.head
      if (index == diffs.indices.max) {
        true
      } else {
        val newValue = value + diffs(index + 1)
        val newList = diffs.take(index) ++ (newValue +: diffs.slice(index + 1, diffs.size).tail)
        isSafe(newList)
      }
    } else if (outliersWithIndices.size == 2) {
      val indices = outliersWithIndices.map(_._2)
      val oneApart = indices.head - indices(1) == 1
      if (oneApart) {
        val newValue = outliersWithIndices.map(_._1).sum
        val newList = diffs.take(outliersWithIndices.head._1) ++ (newValue +: diffs.slice(outliersWithIndices(1)._1, diffs.size).tail)
        isSafe(newList)
      } else {
        false
      }
    } else {
      false
    }
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