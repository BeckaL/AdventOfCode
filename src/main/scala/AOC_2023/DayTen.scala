package AOC_2023

import AOC_2017.DayNineteen.Direction
import shared.{Coord, DayChallenge, TestData}

object DayTen extends DayChallenge[Int, Int] {
  override def partOne(l: List[String]): Int =
    val startCoord = findCoordOfChar(l, 'S')
    List('N', 'E', 'S').flatMap(d =>
      findLoop(0, d, startCoord, startCoord, l).toList
    ).head / 2 + 1

  private def findLoop(loopSizeFar: Int, currentDirection: Char, currentCoordinate: Coord, targetCoordinate: Coord, g: List[String]): Option[Int] =
    val newCoord = currentCoordinate.moveYIsReversed(currentDirection, 1)
    if (newCoord == targetCoordinate && loopSizeFar > 0)
      Some(loopSizeFar)
    else if (!(g.indices.contains(newCoord.y) && g.head.indices.contains(newCoord.x)))
      None
    else
      nextDirection(currentDirection, g(newCoord.y)(newCoord.x)) match
        case None => None
        case Some(direction) =>
          findLoop(loopSizeFar + 1, direction, newCoord, targetCoordinate, g)

  private def nextDirection(directionComingFrom: Char, pipePiece: Char): Option[Char] =
    val pipesToDirectionTransformation = Map(
      '|' -> List(('S', 'S'), ('N', 'N')),
      '-' -> List(('W', 'W'), ('E', 'E')),
      'L' -> List(('S', 'E'), ('W', 'N')),
      'F' -> List(('N', 'E'), ('W', 'S')),
      '7' -> List(('N', 'W'), ('E', 'S')),
      'J' -> List(('S', 'W'), ('E', 'N')),
      '.' -> List()
    )
    pipesToDirectionTransformation(pipePiece).collectFirst
        case (sourceDirection, newDirection) if sourceDirection == directionComingFrom => newDirection

  private def findCoordOfChar(grid: List[String], char: Char) =
    val charsToCoords = grid.zipWithIndex.flatMap((l, y) => l.zipWithIndex.map((char, x) => char -> Coord(x, y))).toMap
    charsToCoords(char)

  override def partTwo(l: List[String]): Int = 
    2
}

object DayTenData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "..F7.",
    ".FJ|.",
    "SJ.L7",
    "|F--J",
    "LJ..."
  )
  override val expectedPartOne: Option[Int] = Some(8)
  override val expectedPartTwo: Option[Int] = Some(0)
}