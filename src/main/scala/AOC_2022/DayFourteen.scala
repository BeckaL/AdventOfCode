package AOC_2022

import shared.{Coord, DayChallenge, TestData}
import shared.UpdaterHelpers._

object DayFourteen extends DayChallenge[Int, Int] {
  override def partOne(l: List[String]): Int =
    dropUntilOverflow(0, startGrid(l, xOverflow = 2, yOverflow = 1))

  override def partTwo(l: List[String]): Int =
    dropUntilBlocked(startGrid(l, xOverflow = 100, yOverflow = 2), 0)

  private def dropUntilBlocked(rows: List[String], count: Int): Int =
    if (cellAtSafe(500, 0, rows).contains('o'))
      count
    else
      dropUntilBlocked(dropSandPart2(Coord(500, 0), rows), count + 1)

  private def dropSandPart2(c: Coord, rows: List[String]): List[String] =
    if (c.y == rows.size - 1)
      rows.update(c.x, c.y, 'o')
    else {
      List((0, 1), (-1, 1), (1, 1)).map(c.move).find { case Coord(x, y) => cellAtSafe(x, y, rows).contains(' ') } match {
        case Some(emptyCoord) => dropSandPart2(emptyCoord, rows)
        case None => rows.update(c.x, c.y, 'o')
      }
    }

  private def dropUntilOverflow(sandDropped: Int, g: List[String]): Int =
    dropSand(Coord(500, 0), g) match {
      case Some(updatedG) => dropUntilOverflow(sandDropped + 1, updatedG)
      case None => sandDropped
    }

  private def dropSand(c: Coord, rows: List[String]): Option[List[String]] = {
    val possibleNewCoords = List((0, 1), (-1, 1), (1, 1)).map(c.move).filter(c => cellAtSafe(c.x, c.y, rows).isDefined)
    if (possibleNewCoords.isEmpty) {
      None //has overflowed
    } else {
      possibleNewCoords.find { case Coord(x, y) => cellAtSafe(x, y, rows).contains(' ') } match {
        case Some(emptyCoord) => dropSand(emptyCoord, rows)
        case None => Some(rows.update(c.x, c.y, 'o')) //has come to resting point
      }
    }
  }

  private def startGrid(l: List[String], xOverflow: Int, yOverflow: Int): List[String] = {
    val coordsToDraw = for {
      line <- l
      points = line.split(" -> ").map(Coord.from)
      (from, to) <- points.dropRight(1).zip(points.tail)
      x <- List(from.x, to.x).min to List(from.x, to.x).max
      y <- List(from.y, to.y).min to List(from.y, to.y).max
    } yield Coord(x, y)

    val (maxX, maxY) = (coordsToDraw.maxBy(_.x).x, coordsToDraw.maxBy(_.y).y)

    val emptyGrid = List.fill(maxY + yOverflow)(List.fill(maxX + xOverflow)(" ").mkString(""))
    coordsToDraw.toSet.foldLeft(emptyGrid) { case (g, Coord(x, y)) => g.update(x, y, 'x') }
  }

  private def cellAtSafe(x: Int, y: Int, l: List[String]): Option[Char] =
    if (l.indices.contains(y) && l.head.indices.contains(x)) Some(l(y)(x)) else None
}


object DayFourteenData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "498,4 -> 498,6 -> 496,6",
    "503,4 -> 502,4 -> 502,9 -> 494,9"
  )
  override val expectedPartOne: Option[Int] = Some(24)
  override val expectedPartTwo: Option[Int] = Some(93)
}