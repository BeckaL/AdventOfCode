package AOC_2017

import shared.{DayChallenge, TestData}

object DayOne extends DayChallenge[Int, Int] {

  override def partOne(l: List[String]): Int = {
    val numbers = getNumbers(l)
    numbers.zipWithIndex.map{case (k, i) =>
    val next = if (i == numbers.size - 1) 0 else i + 1
      if (k == numbers(next)) k else 0
    }.sum
  }

  override def partTwo(l: List[String]): Int = {
    val numbers = getNumbers(l)
    val half = numbers.size / 2
    numbers.take(half).zipWithIndex.map{case (k, i) =>
      if (k == numbers(i + half)) k else 0
    }.sum * 2
  }

  private def getNumbers(l: List[String]): List[Int] = l.head.split("").toList.map(_.toInt)
}

object DayOneData extends TestData[Int, Int] {
  override val testData: List[String] = List("9112122129")
  override val expectedPartOne: Option[Int] = Some(12)
  override val testData2: Option[List[String]] = Some(List("12131415"))
  override val expectedPartTwo: Option[Int] = Some(4)
}
