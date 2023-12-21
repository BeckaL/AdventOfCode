package AOC_2023

import shared.{Coord, DayChallenge, GridHelpers, TestData}

import scala.annotation.tailrec

object DayTwentyOne extends DayChallenge[Int, Int] with GridHelpers {
  override def partOne(l: List[String]): Int =
    val startCoord = l.allCoords.find(c => l(c.y)(c.x) == 'S').get
    val target = if (l == DayTwentyOneData.testData) 6 else 64
    traverse(l, target, Set(startCoord)).size

  @tailrec
  private def traverse(l: List[String], stepsLeftToTake: Int, coords: Set[Coord]): Set[Coord] =
    if (stepsLeftToTake == 0)
      coords
    else
      val newCoords = coords.flatMap(c => c.neighboursWithoutDiagonals.filter(neighbour =>
          l.isInGrid(neighbour) && l(neighbour.y)(neighbour.x) != '#'
      ))
      traverse(l, stepsLeftToTake - 1, newCoords)

  override def partTwo(l: List[String]): Int = ???
}

object DayTwentyOneData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "...........",
    ".....###.#.",
    ".###.##..#.",
    "..#.#...#..",
    "....#.#....",
    ".##..S####.",
    ".##..#...#.",
    ".......##..",
    ".##.#.####.",
    ".##..##.##.",
    "..........."
  )
  override val expectedPartOne: Option[Int] = Some(16)
  override val expectedPartTwo: Option[Int] = Some(6536)
}