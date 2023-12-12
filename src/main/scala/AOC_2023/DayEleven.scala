package AOC_2023

import shared.{DayChallenge, Helpers, TestData, Coord}

object DayEleven extends DayChallenge[Int, Int] with Helpers {
  override def partOne(l: List[String]): Int =
    val expandedWithRows = expandRows(l, List())
    val expandedWithColumns = expandRows(expandedWithRows.transpose.map(_.mkString("")), List()).transpose.map(_.mkString(""))
    findAllPaths(findStars(expandedWithColumns), List()).sum

  override def partTwo(l: List[String]): Int =
    2

  private def findAllPaths(remainingStars: List[Coord], currentPaths: List[Int]): List[Int] =
    remainingStars match
      case lastStar :: Nil => currentPaths
      case nextStar :: otherStars =>
        val newPaths = otherStars.map(_.manhattanDistanceFrom(nextStar))
        findAllPaths(otherStars, currentPaths ++ newPaths)

  private def expandRows(remaining: List[String], newRows: List[String]): List[String] =
    remaining match
      case Nil => newRows
      case firstRow :: newRemaining if firstRow.forall(_ == '.') =>
        val updatedRows = newRows ++ List(firstRow, firstRow)
        expandRows(newRemaining, updatedRows)
      case firstRow :: newRemaining => expandRows(newRemaining, newRows :+ firstRow)

  private def findStars(rows: List[String]): List[Coord] =
    rows.zipWithIndex.flatMap(
      (row, yIndex) => row.zipWithIndex.collect{case (c, xIndex) if c == '#' => Coord(xIndex, yIndex)}
    )
}

object DayElevenData extends TestData[Int, Int] {
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
  override val expectedPartOne: Option[Int] = Some(374)
  override val expectedPartTwo: Option[Int] = Some(0)
}