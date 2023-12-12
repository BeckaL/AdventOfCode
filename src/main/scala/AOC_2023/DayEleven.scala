package AOC_2023

import shared.{DayChallenge, Helpers, TestData, Coord}

object DayEleven extends DayChallenge[Long, Long] with Helpers {
  override def partOne(l: List[String]): Long =
    findAllPaths(findStars(l), emptyRowNumbers(l), emptyColumnNumbers(l), 1L).sum

  override def partTwo(l: List[String]): Long =
    findAllPaths(findStars(l), emptyRowNumbers(l), emptyColumnNumbers(l), 999999L).sum

  private def emptyRowNumbers(l: List[String]) =
    l.indices.filter(y => l(y).mkString("").forall(_ == '.')).toList

  private def emptyColumnNumbers(l: List[String]) =
    l.head.indices.filter(x => l.map(_(x)).mkString("").forall(_ == '.')).toList

  private def findAllPaths(remainingStars: List[Coord], emptyYIndices: List[Int], emptyXIndices: List[Int], multiplier: Long, currentPaths: List[Long] = List()): List[Long] =
    remainingStars match
      case lastStar :: Nil => currentPaths
      case nextStar :: otherStars =>
        val newPaths = otherStars.map(distanceFrom(_, nextStar, emptyYIndices, emptyXIndices, multiplier))
        findAllPaths(otherStars, emptyYIndices, emptyXIndices, multiplier, currentPaths ++ newPaths)

  private def distanceFrom(c1: Coord, c2: Coord, emptyYIndices: List[Int], emptyXIndices: List[Int], multiplier: Long): Long =
    c1.manhattanDistanceFrom(c2) +
    emptyYIndices.count(y => (y > c1.y && y < c2.y) || (y > c2.y && y < c1.y)) * multiplier +
    emptyXIndices.count(x => (x > c1.x && x < c2.x) || (x > c2.x && x < c1.x)) * multiplier

  private def findStars(rows: List[String]): List[Coord] =
    rows.zipWithIndex.flatMap(
      (row, yIndex) => row.zipWithIndex.collect{case (c, xIndex) if c == '#' => Coord(xIndex, yIndex)}
    )
}

object DayElevenData extends TestData[Long, Long] {
  override val testData: List[String] = List(
    "...#......",
    ".......#..",
    "#.........",
    "..........",
    "......#...",
    ".#........",
    ".........#",
    "..........",
    ".......#..",
    "#...#....."
  )
  override val expectedPartOne: Option[Long] = Some(374L)
  override val expectedPartTwo: Option[Long] = None
}