package AOC_2021

import shared.{DayChallenge, TestData}

object DayOne extends DayChallenge[Int, Int] {
  override def partOne(l: List[String]): Int =
    getNumberOfIncreases(l.map(_.toInt))

  override def partTwo(l: List[String]): Int =
    getNumberOfIncreases(l.map(_.toInt).sliding(3).map(_.sum).toList)

  def getNumberOfIncreases(is: List[Int]): Int =
    is.sliding(2).foldLeft(0) { case (counter, intsToCompare) =>
      if (intsToCompare(1) > intsToCompare(0)) counter + 1 else counter
    }

}

object DayOneData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "199",
    "200",
    "208",
    "210",
    "200",
    "207",
    "240",
    "269",
    "260",
    "263"
  )
  override val expectedPartOne: Option[Int] = Some(7)
  override val expectedPartTwo: Option[Int] = Some(5)
}
