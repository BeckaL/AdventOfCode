package AOC_2024

import shared.{Coord, DayChallenge, GridHelpers, TestData}

import scala.annotation.tailrec

object DayEight extends DayChallenge[Int, Int] with GridHelpers {
  override def partOne(l: List[String]): Int =
    countUniqueCoordinatesOfAntinodes(l, getAntennaExactDistanceAway)

  override def partTwo(l: List[String]): Int =
    countUniqueCoordinatesOfAntinodes(l, getAntennaAnyDistanceAway)

  private def countUniqueCoordinatesOfAntinodes(l: List[String], f: PointFinderFunction): Int =
    l.allCoords.collect { case c if l(c.y)(c.x) != '.' => (c, l(c.y)(c.x)) }
      .groupMap(_._2)(_._1).values.toList
      .map { coords =>
        coords.toList.combinations(2).toSet.collect { case List(a, b) =>
          val (xDelta, yDelta) = (a.x - b.x, a.y - b.y)
          f(xDelta, yDelta, a, b, l)
        }.flatten.filter(c => l.isInGrid(c))
      }.toSet.flatten.size

  type PointFinderFunction = (Int, Int, Coord, Coord, List[String]) => Set[Coord]

  private val getAntennaExactDistanceAway: PointFinderFunction =
      (xDiff: Int, yDiff: Int, a: Coord, b: Coord, l: List[String]) =>
        Set(Coord(a.x + xDiff, a.y + yDiff), Coord(b.x - xDiff, b.y - yDiff))

  private val getAntennaAnyDistanceAway: PointFinderFunction =
      (xDiff: Int, yDiff: Int, a: Coord, b: Coord, l: List[String]) =>
        expandUntilOffGrid((xDiff, yDiff), List(), a, l) ++ expandUntilOffGrid((-xDiff, -yDiff), List(), b, l)

  @tailrec
  private def expandUntilOffGrid(xAndYDiff: (Int, Int), soFar: List[Coord], current: Coord, l: List[String]): Set[Coord] =
    if (!l.isInGrid(current))
      soFar.toSet
    else
      val next = Coord(current.x + xAndYDiff._1, current.y + xAndYDiff._2)
      expandUntilOffGrid(xAndYDiff, current +: soFar, next, l)
}

object DayEightData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "............",
    "........0...",
    ".....0......",
    ".......0....",
    "....0.......",
    "......A.....",
    "............",
    "............",
    "........A...",
    ".........A..",
    "............",
    "............"
  )
  override val expectedPartOne: Option[Int] = Some(14)
  override val expectedPartTwo: Option[Int] = Some(34)
}