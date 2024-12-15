package AOC_2023

import AOC_2017.DayNineteen.Direction
import shared.{Coord, DayChallenge, GridHelpers, TestData}

import scala.annotation.tailrec

object DayTen extends DayChallenge[Int, Int] with GridHelpers {
  override def partOne(l: List[String]): Int =
    val (_, _, endMapOfCoordsToPipes) = findLoopFromStartReturningLoopAndStartAndEndDirections(l)
    endMapOfCoordsToPipes.size / 2 + 1

  private def findLoopFromStartReturningLoopAndStartAndEndDirections(l: List[String]): (Char, Char, Map[Coord, Char]) =
    val startCoord = findCoordOfChar(l, 'S')
    List('N', 'E', 'S').flatMap(d =>
      findLoop(Map(), d, startCoord, startCoord, l)
        .map((coordToPipeMap, endDirection) => (d, endDirection, coordToPipeMap))
        .toList
    ).head

  private def findLoop(loopSoFar: Map[Coord, Char], currentDirection: Char, currentCoordinate: Coord, targetCoordinate: Coord, g: List[String]): Option[(Map[Coord, Char], Char)] =
    val newCoord = currentCoordinate.move(currentDirection, 1)
    if (newCoord == targetCoordinate && loopSoFar.nonEmpty)
      Some(loopSoFar, currentDirection)
    else if (!g.isInGrid(newCoord))
      None
    else
      val pipePiece = g(newCoord.y)(newCoord.x)
      val nextDirection = pipesToDirectionTransformation(pipePiece).collectFirst {
        case (sourceDirection, newDirection) if sourceDirection == currentDirection => newDirection
      }
      nextDirection match
        case None => None
        case Some(direction) =>
          findLoop(loopSoFar.updated(newCoord, pipePiece), direction, newCoord, targetCoordinate, g)


  private val jointChars = Set('L', 'J', 'F', '7')
  private val crossingCharPairs = Map('L' -> '7', 'F' -> 'J')

  private def countEnclosedInLoop(gridWithLoop: List[String]) =
    @tailrec
    def go(count: Int, inLoop: Boolean, c: Coord): Int =
      val (newCount, newInLoop, newCoord) = gridWithLoop(c.y)(c.x) match
        case '.' => (if (inLoop) count + 1 else count, inLoop, c)
        case '|' => (count, !inLoop, c)
        case jointChar if jointChars.contains(jointChar) =>
          val (newCoord, crosses) = findCoordOfNextJointAndIfCrosses(c, jointChar, gridWithLoop)
          (count, if (crosses) !inLoop else inLoop, newCoord)
      newCoord.nextCoordInGrid(gridWithLoop) match
        case None => newCount
        case Some(newCoord) if newCoord.x == 0 => go(newCount, false, newCoord)
        case Some(newCoord) => go(newCount, newInLoop, newCoord)

    go(0, false, Coord(0, 0))

  private def findCoordOfNextJointAndIfCrosses(c: Coord, startChar: Char, l: List[String]): (Coord, Boolean) =
    val row = l(c.y)
    val sliceUntilJoint = row.substring(c.x + 1).takeWhile(!jointChars.contains(_))
    val newX = c.x + sliceUntilJoint.length + 1
    val crosses = crossingCharPairs(startChar) == l(c.y)(newX)
    (Coord(newX, c.y), crosses)

  private def findCoordOfChar(grid: List[String], char: Char) =
    val charsToCoords = grid.zipWithIndex.flatMap((l, y) => l.zipWithIndex.map((char, x) => char -> Coord(x, y))).toMap
    charsToCoords(char)

  private val pipesToDirectionTransformation = Map(
    '|' -> List(('S', 'S'), ('N', 'N')),
    '-' -> List(('W', 'W'), ('E', 'E')),
    'L' -> List(('S', 'E'), ('W', 'N')),
    'F' -> List(('N', 'E'), ('W', 'S')),
    '7' -> List(('N', 'W'), ('E', 'S')),
    'J' -> List(('S', 'W'), ('E', 'N')),
    '.' -> List()
  )

  override def partTwo(l: List[String]): Int =
    val (startDir, endDir, endMapOfCoordsToPipes) = findLoopFromStartReturningLoopAndStartAndEndDirections(l)
    val startPieceReplacement = pipesToDirectionTransformation.collectFirst {
      case (pipe, directionTransformations) if directionTransformations.contains((endDir, startDir)) => pipe
    }.get
    val gridWithReplacementChars = l.indices.toList.map(y => l.head.indices.map(x =>
      l(y)(x) match
        case '.' => '.'
        case 'S' => startPieceReplacement
        case pipeChar => if (endMapOfCoordsToPipes.keys.exists(_ == Coord(x, y))) pipeChar else '.'
    ).mkString(""))
    countEnclosedInLoop(gridWithReplacementChars)
}




object DayTenData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "..F7.",
    ".FJ|.",
    "SJ.L7", //S replaced by F
    "|F--J",
    "LJ..."
  )

  //  override val testData2: Option[List[String]] = Some(List(
  //    "FF7FSF7F7F7F7F7F---7",
  //    "L|LJ||||||||||||F--J",
  //    "FL-7LJLJ||||||LJL-77",
  //    "F--JF--7||LJLJ7F7FJ-",
  //    "L---JF-JLJ.||-FJLJJ7",
  //    "|F|F-JF---7F7-L7L|7|",
  //    "|FFJF7L7F-JF7|JL---7",
  //    "7-L-JL7||F7|L7F-7F7|",
  //    "L.L7LFJ|||||FJL7||LJ",
  //    "L7JLJL-JLJLJL--JLJ.L"
  //  ))

  override val testData2: Option[List[String]] = Some(List(
    "......J....",
    ".S-------7.",
    ".|F-----7|.",
    ".||.....||.",
    ".||.....||.",
    ".|L-7.F-J|.",
    ".|..|.|..|.",
    ".L--J.L--J.",
    "..........."
  ))
  override val expectedPartOne: Option[Int] = Some(8)
  override val expectedPartTwo: Option[Int] = Some(4)
}