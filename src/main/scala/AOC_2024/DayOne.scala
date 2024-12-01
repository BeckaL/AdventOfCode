package AOC_2024

import shared.{DayChallenge, TestData}

object DayOne extends DayChallenge[Int, Int] {
  override def partOne(l: List[String]): Int =
    val (firstList, secondList) = getLists(l)
    firstList.sorted.zip(secondList.sorted).map((i, j) => (i-j).abs).sum

  override def partTwo(l: List[String]): Int =
    val (firstList, secondList) = getLists(l)
    firstList.map(i => secondList.count(_ == i) * i).sum

  private def getLists(l: List[String]) =
    val lineSplit = (line: String) => line.split("\\s+")
    (l.map(lineSplit(_)(0).toInt), l.map(lineSplit(_)(1).toInt))
}

object DayOneData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "3   4",
    "4   3",
    "2   5",
    "1   3",
    "3   9",
    "3   3"
  )
  override val expectedPartOne: Option[Int] = Some(11)
  override val expectedPartTwo: Option[Int] = Some(31)
}