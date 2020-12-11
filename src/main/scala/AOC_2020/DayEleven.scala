package AOC_2020

import shared.{DayChallenge, GridHelpers}

object DayEleven extends DayChallenge[Int, Int] with GridHelpers {
  override def partOne(l: List[String]): Int = go(l, getImmediateNeighbours, 3)

  override def partTwo(l: List[String]): Int = go(l, getNextVisibleSeatNeighbours, 4)

  private def go(lastTick: List[String], getNeighbours: (Int, Int, List[String]) => List[Char], occupiedSeatTolerance: Int): Int =
    nextTick(lastTick, getNeighbours, occupiedSeatTolerance) match {
      case newState @ _ if newState == lastTick => newState.flatten.count(isOccupiedSeat)
      case newState @ _ => go(newState, getNeighbours, occupiedSeatTolerance)
    }

  private def nextTick(rows: List[String], getNeighbours: (Int, Int, List[String]) => List[Char], occupiedSeatTolerance: Int): List[String] =
    rows.zipWithIndex.map { case (string, rowIndex) => string.zipWithIndex.map { case (char, colIndex) =>
      char match {
        case '.' => '.'
        case '#' => if (getNeighbours(colIndex, rowIndex, rows).count(isOccupiedSeat) > occupiedSeatTolerance) 'L' else '#'
        case 'L' => if (getNeighbours(colIndex, rowIndex, rows).count(isOccupiedSeat) == 0) '#' else 'L'
      }
    }.mkString("")
    }

  private def getNextVisibleSeatNeighbours(x: Int, y: Int, l: List[String]): List[Char] =
    diagonalVectors.map(getDiagonalSeatGivenVector(x, y, l, _)) ++
      getNextVisibleSeatGivenLineOfSightOnEitherSide(l.rowAt(y).splitAt(x)) ++
      getNextVisibleSeatGivenLineOfSightOnEitherSide(l.colAt(x).splitAt(y))

  private def getDiagonalSeatGivenVector(x: Int, y: Int, l: List[String], vector: (Int, Int)): Char = {
    val (newX, newY) = (x + vector._1, y + vector._2)
    if (!l.isInGrid(newX, newY)) {
      '.'
    } else {
      l(newY)(newX) match {
        case '.' => getDiagonalSeatGivenVector(newX, newY, l, vector)
        case seat @ _ => seat
      }
    }
  }

  private def getNextVisibleSeatGivenLineOfSightOnEitherSide(beforesAndAfters: (List[Char], List[Char])): List[Char] =
    List(beforesAndAfters._1.reverse.find(isSeat).getOrElse('.'), beforesAndAfters._2.tail.find(isSeat).getOrElse('.'))

  private def isSeat(char: Char) = isOccupiedSeat(char) || char == 'L'
  private def isOccupiedSeat(char: Char) = char == '#'

  override val testData: List[String] = List(
    "L.LL.LL.LL",
    "LLLLLLL.LL",
    "L.L.L..L..",
    "LLLL.LL.LL",
    "L.LL.LL.LL",
    "L.LLLLL.LL",
    "..L.L.....",
    "LLLLLLLLLL",
    "L.LLLLLL.L",
    "L.LLLLL.LL")


  override val expectedPartOne: Option[Int] = Some(37)

  override val expectedPartTwo: Option[Int] = Some(26)
}
