package AOC_2023

import shared.{DayChallenge, Helpers, TestData}
import shared.UpdaterHelpers.ListStringUpdater

object DayThirteen extends DayChallenge[Int, Int] with Helpers {
  override def partOne(l: List[String]): Int =
    splitIntoGroupsOfList(l).map(findReflection(_, 0)).sum

  private def isReflection(firstIndex: Int, g: List[String], diff: Int) =
    val (first, second) = g.splitAt(firstIndex + 1)
    first.reverse.zip(second).map((s1, s2) => s1.zip(s2).count((c1, c2) => c1 != c2)).sum == diff

  private def findReflection(g: List[String], diff: Int) =
    (0 until g.indices.max).find(isReflection(_, g, diff)) match
      case Some(i) => (i + 1) * 100
      case None =>
        val transposed = g.transpose.map(_.mkString("").reverse)
        (0 until transposed.indices.max).find(isReflection(_, transposed, diff)).get + 1

  override def partTwo(l: List[String]): Int =
    splitIntoGroupsOfList(l).map(findReflection(_, 1)).sum
}

object DayThirteenData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "#.##..##.",
    "..#.##.#.",
    "##......#",
    "##......#",//3
    "..#.##.#.",//4
    "..##..##.",
    "#.#.##.#.", //6
    "",
    "#...##..#",
    "#....#..#",
    "..##..###",
    "#####.##.",
    "#####.##.",
    "..##..###",
    "#....#..#",
  )
  override val expectedPartOne: Option[Int] = Some(405)
  override val expectedPartTwo: Option[Int] = Some(400)
}