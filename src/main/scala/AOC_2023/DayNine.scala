package AOC_2023

import shared.{DayChallenge, Helpers, TestData}

object DayNine extends DayChallenge[Int, Int] with Helpers {
  override def partOne(l: List[String]): Int =
    getStartSequence(l).map(getNextInSequence).sum

  override def partTwo(l: List[String]): Int =
    getStartSequence(l).map(getPreviousInSequence).sum

  private def getNextInSequence(numbers: List[List[Int]]): Int =
    expandSequence(numbers).foldLeft(0)((previousDiff, numberLine) => numberLine.last + previousDiff)

  private def getPreviousInSequence(numbers: List[List[Int]]): Int =
    expandSequence(numbers).foldLeft(0)((previousDiff, numberLine) => numberLine.head - previousDiff)

  private def expandSequence(numbers: List[List[Int]]): List[List[Int]] =
    val diffs = numbers.head.dropRight(1).zip(numbers.head.drop(1)).map((i1, i2) => i2 - i1)
    if (diffs.forall(_ == 0)) numbers else expandSequence(diffs +: numbers)

  private def getStartSequence(input: List[String]) =
    input.map(_.split(" ").toList.map(interpretNumberWithOptionalSign)).map(List(_))
}

object DayNineData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "0 3 6 9 12 15",
    "1 3 6 10 15 21",
    "10 13 16 21 30 45"
  )

  override val expectedPartOne: Option[Int] = Some(114)
  override val expectedPartTwo: Option[Int] = Some(2)
}