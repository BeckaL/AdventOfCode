package AOC_2024

import shared.{Coord, DayChallenge, GridHelpers, TestData}

object DayTen extends DayChallenge[Int, Int] with GridHelpers{
  override def partOne(l: List[String]): Int =
    l.allCoords
      .filter(c => l(c.y)(c.x) == '0')
      .map(countPathEndsFromC(_, l).size).sum

  override def partTwo(l: List[String]): Int =
    l.allCoords
      .filter(c => l(c.y)(c.x) == '0')
      .map(countDistinctPaths(_, l)).sum

  private def countPathEndsFromC(c: Coord, l: List[String], next: Int = 1): Set[Coord] =
    val validNexts = getValidNexts(c, l, next)
    if (validNexts.isEmpty)
      Set()
    else if (next == 9)
      validNexts
    else
      validNexts.flatMap(countPathEndsFromC(_, l, next + 1))

  private def countDistinctPaths(c: Coord, l: List[String], next: Int = 1): Int =
    val validNexts = getValidNexts(c, l, next).toList
    if (validNexts.isEmpty)
        0
    else if (next == 9)
      validNexts.size
    else
      validNexts.map(countDistinctPaths(_, l, next + 1)).sum

  private def getValidNexts(c: Coord, l: List[String], next: Int) =
    c.neighboursWithoutDiagonals.filter(nextC =>
      l.isInGrid(nextC) && l(nextC.y)(nextC.x).toString.toInt == next
    )
}

object DayTenData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "89010123",
    "78121874",
    "87430965",
    "96549874",
    "45678903",
    "32019012",
    "01329801",
    "10456732"
  )
  override val expectedPartOne: Option[Int] = Some(36)
  override val expectedPartTwo: Option[Int] = Some(81)
}