package AOC_2025

import shared.{Coord, DayChallenge, TestData}

import scala.annotation.tailrec

object DayFour extends DayChallenge[Int, Int] {
  override def partOne(l: List[String]): Int =
    val boxes = coordsOfBoxes(l)
    boxes.count(c => countNeighbours(c, boxes) < 4)

  override def partTwo(l: List[String]): Int =
    countAccesibleBoxesRemoving(coordsOfBoxes(l), 0)

  private def coordsOfBoxes(l: List[String]): Set[Coord] =
    l.zipWithIndex.flatMap {
      case (row, y) => row.zipWithIndex.map {
        case (char, x) => (Coord(x, y), char)
      }
    }.collect { case (coord, char) if char == '@' => coord }.toSet

  private def countNeighbours(coord: Coord, all: Set[Coord]): Int =
    all.intersect(coord.neighbours).size

  @tailrec
  private def countAccesibleBoxesRemoving(coordsOfBoxes: Set[Coord], removedSoFar: Int): Int =
    val boxesToRemove = coordsOfBoxes.filter(c => countNeighbours(c, coordsOfBoxes) < 4)
    if (boxesToRemove.isEmpty)
      removedSoFar
    else
      val remainingBoxes = coordsOfBoxes.diff(boxesToRemove)
      countAccesibleBoxesRemoving(remainingBoxes, removedSoFar + boxesToRemove.size)
}

object DayFourData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "..@@.@@@@.",
    "@@@.@.@.@@",
    "@@@@@.@.@@",
    "@.@@@@..@.",
    "@@.@@@@.@@",
    ".@@@@@@@.@",
    ".@.@.@.@@@",
    "@.@@@.@@@@",
    ".@@@@@@@@.",
    "@.@.@@@.@."
  )
  override val expectedPartOne: Option[Int] = Some(13)
  override val expectedPartTwo: Option[Int] = Some(43)
}