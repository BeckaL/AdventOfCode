package AOC_2022

import shared.{DayChallenge, TestData}

object DayOne extends DayChallenge[Int, Int] {
  override def partOne(l: List[String]): Int = parse(l).max

  override def partTwo(l: List[String]): Int = parse(l).sorted(Ordering[Int].reverse).take(3).sum

  private def parse(l: List[String]): List[Int] =
    l.mkString(",").split(",,").toList.map(x => x.split(",").map(_.toInt).sum)
}

object DayOneData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "1000",
    "2000",
    "3000",
    "",
    "4000",
    "",
    "5000",
    "6000",
    "",
    "7000",
    "8000",
    "9000",
    "",
    "10000",
  )
  override val expectedPartOne: Option[Int] = Some(24000)
  override val expectedPartTwo: Option[Int] = Some(45000)
}
