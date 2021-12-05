package AOC_2021

import shared.{Coord, DayChallenge, Helpers, TestData}

object DayFive extends DayChallenge[Int, Int] with Helpers {
  override def partOne(l: List[String]): Int =
    noOfCoordsAppearingMoreThanOnceFrom(l.map(Line.from).filter(_.isStraight))

  override def partTwo(l: List[String]): Int =
    noOfCoordsAppearingMoreThanOnceFrom(l.map(Line.from))

  private def noOfCoordsAppearingMoreThanOnceFrom(lines: List[Line]) =
    lines.flatMap(_.coordsCovered)
      .groupBy(identity).collect {
        case (coord, grouped) if grouped.size > 1 => coord
      }.size
}

case class Line(from: Coord, to: Coord) extends Helpers {
  val isStraight: Boolean = from.x == to.x || from.y == to.y

  def coordsCovered: List[Coord] = {
    val xsCovered = rangeIncreasingOrDecreasing(from.x, to.x)
    val ysCovered = rangeIncreasingOrDecreasing(from.y, to.y)
    if (from.x == to.x) {
      ysCovered.map(yCoord => Coord(from.x, yCoord))
    } else if (from.y == to.y) {
      xsCovered.map(xCoord => Coord(xCoord, from.y))
    } else {
      xsCovered.zip(ysCovered).map { case (x, y) => Coord(x, y) }
    }
  }
}

object Line extends Helpers {
  def from(str: String) = {
    val (start, end) = getTwoFromSplit(str, " -> ")
    Line(Coord.from(start), Coord.from(end))
  }
}

object DayFiveData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "0,9 -> 5,9",
    "8,0 -> 0,8",
    "9,4 -> 3,4",
    "2,2 -> 2,1",
    "7,0 -> 7,4",
    "6,4 -> 2,0",
    "0,9 -> 2,9",
    "3,4 -> 1,4",
    "0,0 -> 8,8",
    "5,5 -> 8,2"
  )
  override val expectedPartOne: Option[Int] = Some(5)
  override val expectedPartTwo: Option[Int] = Some(12)
}
