package AOC_2022

import shared.{Coord, DayChallenge, TestData}

object DayTwelve extends DayChallenge[Int, Int] {
  override def partOne(l: List[String]): Int = {
    val (start, end, rows) = parse(l)
    findMinPath(start, end, rows).get
  }

  override def partTwo(l: List[String]): Int = {
    val (_, end, rows) = parse(l)
    allStarts(rows, List()).flatMap(findMinPath(_, end, rows)).min
  }

  private def parse(l: List[String]): (Coord, Coord, List[String]) = {
    val (startCoord, updatedWithStart) = findCoordAndUpdateRows('S', 'a', l, List(), 0)
    val (endCoord, updatedWithEnd) = findCoordAndUpdateRows('E', 'z', updatedWithStart, List(), 0)
    (startCoord, endCoord, updatedWithEnd)
  }

  private def allStarts(rows: List[String], found: List[Coord], rowNo: Int = 0): List[Coord] =
    rows match {
      case Nil => found
      case head :: others =>
        val xIndexes = head.zipWithIndex.filter { case (char, _) => char == 'a' }.map(_._2)
        allStarts(others, found ++ xIndexes.map(x => Coord(x, rowNo)), rowNo + 1)
    }

  private def findMinPath(start: Coord, endPosition: Coord, grid: List[String]): Option[Int] = {
    def go(toExplore: List[Coord], nextLevel: Set[Coord], steps: Int, visited: Set[Coord]): Option[Int] =
      (toExplore, nextLevel) match {
        case (next :: others, _) =>
          val currentValue: Char = grid(next.y)(next.x)
          val validNeighbours = neighboursInGrid(grid, next)
            .filter { case Coord(x, y) => grid(y)(x).toInt - currentValue.toInt <= 1 }
            .map { case Coord(x, y) => Coord(x, y) }
            .filterNot(visited.contains)
          val newNextLevel = nextLevel ++ validNeighbours.toSet
          if (newNextLevel.contains(endPosition))
            Some(steps + 1)
          else
            go(others, newNextLevel, steps, visited + next)
        case (Nil, next) if next.isEmpty => None
        case (Nil, _) => go(nextLevel.toList, Set(), steps + 1, visited)
      }

    go(List(start), Set(), 0, Set())
  }

  private def neighboursInGrid(g: List[String], c: Coord): List[Coord] =
    List((-1, 0), (0, -1), (0, 1), (1, 0))
      .map(v => (c.x + v._1, c.y + v._2))
      .filter { case (x, y) => g.indices.contains(Coord(x, y).y) && g.head.indices.contains(Coord(x, y).x) }
      .map { case (x, y) => Coord(x, y) }

  private def findCoordAndUpdateRows(toFind: Char, replace: Char, rowsToSearch: List[String], traversedRows: List[String], rowNo: Int): (Coord, List[String]) = {
    val row = rowsToSearch.head
    row.indexOf(toFind) match {
      case -1 => findCoordAndUpdateRows(toFind, replace, rowsToSearch.tail, traversedRows :+ row, rowNo + 1)
      case i =>
        val (before, after) = row.splitAt(i)
        val newRow = (before :+ replace) ++ after.tail
        val updatedRows = if (rowsToSearch.size > 1)
          (traversedRows :+ newRow) ++ rowsToSearch.tail
        else
          traversedRows :+ newRow
        (Coord(i, rowNo), updatedRows)
    }
  }
}

object DayTwelveData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "Sabqponm",
    "abcryxxl",
    "accszExk",
    "acctuvwj",
    "abdefghi"
  )
  override val expectedPartOne: Option[Int] = Some(31)
  override val expectedPartTwo: Option[Int] = Some(29)
}