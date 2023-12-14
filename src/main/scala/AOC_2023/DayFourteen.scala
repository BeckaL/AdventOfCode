package AOC_2023

import shared.{DayChallenge, GridHelpers, TestData, Coord}
import shared.UpdaterHelpers.ListStringUpdater

object DayFourteen extends DayChallenge[Int, Int] with GridHelpers {
  override def partOne(l: List[String]): Int =
    getScore(moveRocks(l)).sum

  override def partTwo(l: List[String]): Int =
    val (pattern, offset) = detectPattern(l)
    pattern((1000000000 - (offset + 1)) % pattern.size)

  private def getScore(updatedGrid: List[String]) =
    updatedGrid.allCoords.map { case Coord(x, y) =>
      if (updatedGrid(y)(x) == 'O') updatedGrid.size - y else 0
    }

  private def moveRocks(l: List[String]): List[String] =
    l.allCoords.foldLeft(l) { case (currentGrid, coord) =>
      if (currentGrid(coord.y)(coord.x) == 'O')
        val column = (0 until coord.y).map(currentGrid(_)(coord.x)).toList.reverse
        val index = findIndexToMoveTo(column)
        if (index == coord.y) currentGrid else currentGrid.update(coord.x, coord.y, '.').update(coord.x, index, 'O')
      else currentGrid
    }

  private def moveRocksSouth(l: List[String]): List[String] =
    l.allCoords.sortBy(c => (-c.y, c.x)).foldLeft(l) { case (currentGrid, coord) =>
      if (currentGrid(coord.y)(coord.x) == 'O')
        val column = (coord.y until currentGrid.size).map(currentGrid(_)(coord.x)).toList.tail
        val index = coord.y + column.length - findIndexToMoveTo(column)
        if (index == coord.y) currentGrid else currentGrid.update(coord.x, coord.y, '.').update(coord.x, index, 'O')
      else currentGrid
    }

  private def moveRocksWest(l: List[String]): List[String] =
    l.allCoords.sortBy(c => (c.y, c.x)).foldLeft(l) { case (currentGrid, coord) =>
      if (currentGrid(coord.y)(coord.x) == 'O')
        val row = currentGrid(coord.y).substring(0, coord.x).toList.reverse
        val index = findIndexToMoveTo(row)
        if (index == coord.x) currentGrid else currentGrid.update(coord.x, coord.y, '.').update(index, coord.y, 'O')
      else currentGrid
    }

  private def moveRocksEast(l: List[String]): List[String] =
    l.allCoords.sortBy(c => (-c.x, c.y)).foldLeft(l) { case (currentGrid, coord) =>
      if (currentGrid(coord.y)(coord.x) == 'O')
        val row = currentGrid(coord.y).substring(coord.x, l.head.length).toList.tail
        val index = coord.x + row.length - findIndexToMoveTo(row)
        if (index == coord.x) currentGrid else currentGrid.update(coord.x, coord.y, '.').update(index, coord.y, 'O')
      else currentGrid
    }

  private def roll(l: List[String]): List[String] =
    List(moveRocks, moveRocksWest, moveRocksSouth, moveRocksEast).foldLeft(l) { case (grid, f) => f(grid) }

  private def findIndexToMoveTo(reversedColumn: List[Char]): Int =
    reversedColumn match
      case Nil => 0
      case '.' :: others => findIndexToMoveTo(others)
      case otherChar :: _ => reversedColumn.size

  private def detectPattern(l: List[String]): (List[Int], Int) =
    val found: Option[(List[Int], Int)] = None
    val (_, _, maybeFoundPattern) = (1 to 500).foldLeft((List[Int](), l, found)) { case ((soFar, grid, found), i) =>
      if (found.isDefined) {
        (soFar, grid, found)
      } else {
        val updatedGrid = roll(grid)
        val score = getScore(updatedGrid).sum
        val updatedList = score +: soFar
        if (i > 50) {
          val foundLengthOfPattern = (5 until 20).find(length => updatedList.take(length) == updatedList.slice(length, length * 2))
          foundLengthOfPattern match
            case Some(length) =>
              val pattern = updatedList.take(length).reverse
              (updatedList, updatedGrid, Some(pattern, i))
            case None => (updatedList, updatedGrid, found)
        } else {
          (updatedList, updatedGrid, found)
        }
      }
    }
    maybeFoundPattern match
      case Some((l, i)) => (l, i)
      case None => throw new RuntimeException("no pattern detected")
}

object DayFourteenData extends TestData[Int, Int] {
  override val testData: List[String] = List(
        "O....#....", //0
        "O.OO#....#", //1
        ".....##...", //2
        "OO.#O....O", //3
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