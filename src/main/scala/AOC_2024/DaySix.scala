package AOC_2024

import shared.{Coord, DayChallenge, Direction, GridHelpers, TestData}

import scala.annotation.tailrec

object DaySix extends DayChallenge[Int, Int] with GridHelpers {
  override def partOne(l: List[String]): Int =
    val startCoord = getStartCoord(l)
    numberOfUniqueSteps(startCoord, 'N', l, Set((startCoord, 'N'))).get

  @tailrec
  private def numberOfUniqueSteps(c: Coord, direction: Char, l: List[String], soFar: Set[(Coord, Char)]): Option[Int] =
    val newCoord = c.moveYIsReversed(direction, 1)
    if (!l.isInGrid(newCoord))
      Some(soFar.map(_._1).size)
    else if (soFar.contains((newCoord, direction)))
      None
    else if (Set('#', '0').contains(l(newCoord.y)(newCoord.x)))
      numberOfUniqueSteps(c, Direction.rotateClockwise(direction), l, soFar)
    else
      numberOfUniqueSteps(newCoord, direction, l, soFar ++ Set((newCoord, direction)))

  private def getStartCoord(l: List[String]): Coord =
    val (row, startY) = l.zipWithIndex.find((row, y) => row.contains("^")).get
    Coord(row.indexOf("^"), startY)

  private def createsAnObstruction(c: Coord, l: List[String], startC: Coord): Boolean =
    val newL = l.updated(c.y, l(c.y).updated(c.x, '0'))
    numberOfUniqueSteps(startC, 'N', newL, Set((startC, 'N'))).isEmpty

  override def partTwo(l: List[String]): Int =
    l.allCoords.count(c =>
      !Set('#', '^').contains(l(c.y)(c.x)) && createsAnObstruction(c, l, getStartCoord(l))
    )
}

object DaySixData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "....#.....",
    ".........#",
    "..........",
    "..#.......",
    ".......#..",
    "..........",
    ".#..^.....",
    "........#.",
    "#.........",
    "......#..."
  )
  override val expectedPartOne: Option[Int] = Some(41)
  override val expectedPartTwo: Option[Int] = Some(6)
}