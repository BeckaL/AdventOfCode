package AOC_2022

import shared.{DayChallenge, TestData}

object DaySix extends DayChallenge[Int, Int]{
  override def partOne(l: List[String]): Int = findFirstUniqueSeqOfNChar(4, l.head)

  override def partTwo(l: List[String]): Int = findFirstUniqueSeqOfNChar(14, l.head)

  private def findFirstUniqueSeqOfNChar(n: Int, s: String, i: Int = 0): Int =
    if (s.slice(i, i + n).distinct.length == n) i + n else findFirstUniqueSeqOfNChar(n, s, i + 1)
}

object DaySixData extends TestData[Int, Int] {
  override val testData: List[String] = List("mjqjpqmgbljsphdztnvjfqwrcgsmlb")
  override val expectedPartOne: Option[Int] = Some(7)
  override val expectedPartTwo: Option[Int] = Some(19)
}