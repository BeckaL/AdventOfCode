package AOC_2024

import shared.{DayChallenge, Helpers, TestData}

object DayThree extends DayChallenge[Long, Long] with Helpers{
  override def partOne(l: List[String]): Long =
    l.map(getSumOfMultiplyInstructions).sum

  private def getSumOfMultiplyInstructions(s: String): Long =
    val results = """mul\((\d+),(\d+)\)""".r.findAllIn(s).toList
    results.map { s =>
      val ints = extractInts(s)
      ints.head.toLong * ints(1).toLong
    }.sum

  override def partTwo(l: List[String]): Long = {
    2
  }
}

object DayThreeData extends TestData[Long, Long] {
  override val testData: List[String] = List(
    "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
  )
  override val testData2: Option[List[String]] = Some(List("xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"))
  override val expectedPartOne: Option[Long] = Some(161)
  override val expectedPartTwo: Option[Long] = Some(48)
}