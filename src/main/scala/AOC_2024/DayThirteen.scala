package AOC_2024

import shared.{DayChallenge, Helpers, TestData}

object DayThirteen extends DayChallenge[Long, Long] with Helpers{
  override def partOne(l: List[String]): Long =
    splitIntoGroupsOfList(l).map(parseButtonsAndPrizes(_)).map(solveLinearEquations).sum

  private def solveLinearEquations(a: (Int, Int), b: (Int, Int), target: (Long, Long)) =
    val aAnswer = BigDecimal(target._1 * b._2 - target._2 * b._1) / BigDecimal(a._1 * b._2 - a._2 * b._1)
    val bAnswer = BigDecimal(a._1 * target._2 - a._2 * target._1) / BigDecimal(a._1 * b._2 - a._2 * b._1)
    if (isWholeNumber(aAnswer) && isWholeNumber(bAnswer))
      (aAnswer * 3 + bAnswer).toLong
    else
      0

  private def isWholeNumber(n: BigDecimal) =
    n.remainder(BigDecimal(1)).compareTo(BigDecimal(0)) == 0

  private def parseButtonsAndPrizes(s: List[String], targetAddition: Long = 0): ((Int, Int), (Int, Int), (Long, Long)) =
    val is = extractInts(s.mkString("\n"))
    ((is.head, is(1)), (is(2), is(3)), (is(4) + targetAddition, is(5) + targetAddition))

  override def partTwo(l: List[String]): Long =
    splitIntoGroupsOfList(l).map(parseButtonsAndPrizes(_, 10000000000000L))
      .map(solveLinearEquations).sum
}

object DayThirteenData extends TestData[Long, Long] {
  override val testData: List[String] = List(
    "Button A: X+94, Y+34",
    "Button B: X+22, Y+67",
    "Prize: X=8400, Y=5400",
    "",
    "Button A: X+26, Y+66",
    "Button B: X+67, Y+21",
    "Prize: X=12748, Y=12176",
    "",
    "Button A: X+17, Y+86",
    "Button B: X+84, Y+37",
    "Prize: X=7870, Y=6450",
    "",
    "Button A: X+69, Y+23",
    "Button B: X+27, Y+71",
    "Prize: X=18641, Y=10279"
  )
  override val expectedPartOne: Option[Long] = Some(480L)
  override val expectedPartTwo: Option[Long] = Some(875318608908L)
}