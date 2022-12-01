package AOC_2022

import shared.{DayChallenge, TestData}

object DayTwo extends DayChallenge[Int, Int]{
  override def partOne(l: List[String]): Int = {
    1
  }

  override def partTwo(l: List[String]): Int = {
    2
  }
}

object DayTwoData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    """"""
  )
  override val expectedPartOne: Option[Int] = Some(0)
  override val expectedPartTwo: Option[Int] = Some(-)
}
