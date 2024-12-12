package AOC_2024

import shared.{Coord, DayChallenge, GridHelpers, TestData}

import scala.annotation.tailrec

object DayTwelve extends DayChallenge[Long, Long] with GridHelpers {
  override def partOne(l: List[String]): Long =
    getRegions(l.allCoords, l).map(r => r.size.toLong * allEdges(r).size).sum

  @tailrec
  private def getRegions(coordsRemaining: List[Coord], l: List[String], found: List[Set[Coord]] = List()): List[Set[Coord]] =
    coordsRemaining match
      case Nil => found
      case first :: others =>
        val foundRegion = bucketFill(l, Set(first), List(first), l(first.y)(first.x))
        val newCoordsRemaining = coordsRemaining.filterNot(foundRegion.contains)
        getRegions(newCoordsRemaining, l, foundRegion +: found)

  private def allEdges(r: Set[Coord]): List[(Coord, Char)] =
    r.toList.flatMap(c => List('N', 'S', 'E', 'W').map((c, _)))
      .filterNot{ case (c, char) => r.contains(c.moveYIsReversed(char, 1))}

  private def countContinguousEdges(edges: List[(Coord, Char)]) =
    val nsFilter = (c1: Coord, c2: Coord) => c1.y == c2.y
    val ewFilter = (c1: Coord, c2: Coord) => c1.x == c2.x
    @tailrec
    def go(remainingEdges: List[(Coord, Char)], soFar: Int): Int =
      remainingEdges match
        case Nil => soFar
        case (nextCoord, edge) :: otherRemaining =>
          val pred = if (Set('N', 'S').contains(edge)) nsFilter else ewFilter
          val edgesInLine = remainingEdges.filter((c, e) => e == edge && pred(c, nextCoord))
          val contiguousCells = getContiguousCells(nextCoord, edgesInLine.map(_._1)).map((_, edge))
          go(remainingEdges.filterNot(contiguousCells.contains), soFar + 1)

    go(edges, 0)

  private def getContiguousCells(start: Coord, toCheck: List[Coord]): List[Coord] =
    @tailrec
    def go(found: List[Coord], toExplore: List[Coord], current: Coord, i: Int): List[Coord] =
      val additionalFound = current.neighboursWithoutDiagonals.filter(neighbour =>
        toCheck.contains(neighbour) && !found.contains(neighbour))
      val newToExplore = toExplore ++ additionalFound
      if (newToExplore.isEmpty)
        found ++ additionalFound
      else
        go(found ++ additionalFound, newToExplore.tail, newToExplore.head, i + 1)

    go(List(start), List(), start, 0)

  @tailrec
  private def bucketFill(l: List[String], found: Set[Coord], next: List[Coord], char: Char): Set[Coord] =
    next match
      case Nil => found
      case nextCoord :: remainingNext =>
        val additionalNeighbours = getNeighbourCoordNoDiagonals(nextCoord, l)
          .filter(c => l(c.y)(c.x) == char && !found.contains(c))
        val newNext = next.tail ++ additionalNeighbours
        bucketFill(l, found ++ additionalNeighbours.toSet, newNext, char)

  override def partTwo(l: List[String]): Long =
    getRegions(l.allCoords, l)
      .map(r => r.size.toLong * countContinguousEdges(allEdges(r))).sum
}

object DayTwelveData extends TestData[Long, Long] {
  override val testData: List[String] = List(
    "RRRRIICCFF",
    "RRRRIICCCF",
    "VVRRRCCFFF",
    "VVRCCCJFFF",
    "VVVVCJJCFE",
    "VVIVCCJJEE",
    "VVIIICJJEE",
    "MIIIIIJJEE",
    "MIIISIJEEE",
    "MMMISSJEEE"
  )
  override val expectedPartOne: Option[Long] = Some(1930)
  override val expectedPartTwo: Option[Long] = Some(1206)
}