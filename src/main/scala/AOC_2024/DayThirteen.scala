package AOC_2024

import shared.{DayChallenge, Helpers, TestData}

object DayThirteen extends DayChallenge[Int, Int] with Helpers{
  override def partOne(l: List[String]): Int =
    splitIntoGroupsOfList(l).map(parseButtonsAndPrizes).flatMap(optimumPresses).sum

  private def optimumPresses(a: (Int, Int), b: (Int, Int), target: (Int, Int)): Option[Int] =
    val possibleTokensSpent = (1 to 100).flatMap(aPresses =>
      (0 to 100).find(bPresses =>
        (a._1 * aPresses) + (b._1 * bPresses) == target._1 &&
          (a._2 * aPresses) + (b._2 * bPresses) == target._2
      ).map(bPresses => aPresses * 3 + bPresses).toList
    )
    possibleTokensSpent.find(i => i == possibleTokensSpent.min)

  private def parseButtonsAndPrizes(s: List[String]): ((Int, Int), (Int, Int), (Int, Int)) =
    val is = extractInts(s.mkString("\n"))
    ((is.head, is(1)), (is(2), is(3)), (is(4), is(5)))

  override def partTwo(l: List[String]): Int =
    2
}

object DayThirteenData extends TestData[Int, Int] {
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
  override val expectedPartOne: Option[Int] = Some(480)
  override val expectedPartTwo: Option[Int] = Some(0)
}