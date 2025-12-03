package AOC_2025

import shared.{DayChallenge, TestData}

import scala.annotation.tailrec

object DayThree extends DayChallenge[Int, Long]{
  override def partOne(l: List[String]): Int =
    l.map(line => getLargestVoltage(line.map(_.toString.toInt).toList, 2)).sum.toInt

  override def partTwo(l: List[String]): Long =
    l.map(line => getLargestVoltage(line.map(_.toString.toInt).toList, 12)).sum

  @tailrec
  private def getLargestVoltage(ints: List[Int], targetDigits: Int, soFar: String = ""): Long =
    val remainingToGo = targetDigits - soFar.length
    if (remainingToGo == 0)
      soFar.toLong
    else
      val inScopeForFirstDigit = ints.dropRight(remainingToGo - 1)
      val largest = inScopeForFirstDigit.max
      val remainingInScope = ints.drop(ints.indexOf(largest) + 1)
      getLargestVoltage(remainingInScope, targetDigits, soFar + largest.toString)
}

object DayThreeData extends TestData[Int, Long] {
  override val testData: List[String] = List(
    "987654321111111","811111111111119","234234234234278","818181911112111"
  )
  override val expectedPartOne: Option[Int] = Some(357)
  override val expectedPartTwo: Option[Long] = Some(3121910778619L)
}