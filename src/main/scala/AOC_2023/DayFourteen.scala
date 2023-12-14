package AOC_2023

import shared.{DayChallenge, GridHelpers, TestData, Coord}
import shared.UpdaterHelpers.ListStringUpdater

object DayFourteen extends DayChallenge[Int, Int] with GridHelpers {
  override def partOne(l: List[String]): Int = getScore(moveRocks(l))

  override def partTwo(l: List[String]): Int = getNthInPattern(l, 1000000000)

  private def getScore(updatedGrid: List[String]) =
    updatedGrid.allCoords.map { case Coord(x, y) =>
      if (updatedGrid(y)(x) == 'O') updatedGrid.size - y else 0
    }.sum

  private def moveRocks(l: List[String]): List[String] =
    l.allCoords.foldLeft(l) { case (currentGrid, coord) =>
      if (currentGrid(coord.y)(coord.x) == 'O')
        val column = (0 until coord.y).map(currentGrid(_)(coord.x)).toList.reverse
        val index = column.find(_ != '.').map(column.size - column.indexOf(_)).getOrElse(0)
        if (index == coord.y)
          currentGrid
        else
          currentGrid.update(coord.x, coord.y, '.').update(coord.x, index, 'O')
      else currentGrid
    }

  private def roll(l: List[String]) = (0 until 4).foldLeft(l) { case (g, _) => moveRocks(g).rotate90 }

  private def getNthInPattern(l: List[String], n: Int, soFar: List[Int] = List()): Int =
    val newL = roll(l)
    val updatedList = getScore(newL) +: soFar
    val maybePattern = if (soFar.size > 40)
      (5 until 20).find(length => updatedList.take(length) == updatedList.slice(length, length * 2))
        .map(lengthOfPattern =>
          (updatedList.take(lengthOfPattern).reverse, updatedList.size))
    else None
    maybePattern match
      case Some(pattern, offset) => pattern((1000000000 - (offset + 1)) % pattern.size)
      case None => getNthInPattern(newL, n, updatedList)
}

object DayFourteenData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "O....#....",
    "O.OO#....#",
    ".....##...",
    "OO.#O....O",
    ".O.....O#.",
    "O.#..O.#.#",
    "..O..#O..O",
    ".......O..",
    "#....###..",
    "#OO..#...."
  )
  override val expectedPartOne: Option[Int] = Some(136)
  override val expectedPartTwo: Option[Int] = Some(64)
}