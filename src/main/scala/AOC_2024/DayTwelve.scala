package AOC_2024

import shared.{Coord, DayChallenge, GridHelpers, TestData}

import scala.annotation.tailrec

object DayTwelve extends DayChallenge[Long, Long] with GridHelpers {
  override def partOne(l: List[String]): Long =
    val regions = getRegions(l.allCoords.toList, List(), l)
    regions.map(r => r.size.toLong * getPerimeter(r, l)).sum

  @tailrec
  private def getRegions(coordsRemaining: List[Coord], found: List[Set[Coord]], l: List[String]): List[Set[Coord]] =
    coordsRemaining match
      case Nil => found
      case first :: others =>
        val char = l(first.y)(first.x)
        val foundRegion = bucketFill(l, Set(first), List(first), char)
        val newCoordsRemaining = coordsRemaining.filterNot(foundRegion.contains)
        getRegions(newCoordsRemaining, foundRegion +: found, l)

  private def getPerimeter(region: Set[Coord], l: List[String]): Int =
    region.toList.map(c => 4 - region.toList.count(otherCoord =>
      otherCoord != c &&
        getNeighbourCoordNoDiagonals(c.x, c.y, l).map{case (x, y) => Coord(x, y)}.contains(otherCoord))).sum

  private def getNeighbourCoords(c: Coord, l: List[String]): List[Coord] =
    getNeighbourCoordNoDiagonals(c.x, c.y, l).map { case (x, y) => Coord(x, y) }

  private def allEdges(r: Set[Coord]): List[(Coord, Char)] =
    r.toList
      .flatMap(c => List('N', 'S', 'E', 'W').map((c, _)))
      .filterNot{ case (c, char) =>
        char match
          case 'N' => r.contains(Coord(c.x, c.y - 1))
          case 'S' => r.contains(Coord(c.x, c.y + 1))
          case 'E' => r.contains(Coord(c.x + 1, c.y))
          case _ => r.contains(Coord(c.x - 1, c.y))
    }

  @tailrec
  private def countContinguousEdges(edges: List[(Coord, Char)], soFar: Int): Int =
    if (edges.isEmpty) {
      soFar
    } else {
      val (nextCoord, nextEdge) = edges.head
      val possibleContiguousEdges = edges.filter(e => e._2 == nextEdge)
      val contiguousCells = nextEdge match {
        case northOrSouth if Set('N', 'S').contains(northOrSouth) =>
          val cc = getContiguousCells(nextCoord, possibleContiguousEdges.filter(_._1.y == nextCoord.y).map(_._1))
          cc.map(c => (c, northOrSouth))
        case dir =>
          val cc = getContiguousCells(nextCoord, possibleContiguousEdges.filter(_._1.x == nextCoord.x).map(_._1))
          cc.map(c => (c, dir))
      }
      val newEdges = edges.filterNot(contiguousCells.contains)
      countContinguousEdges(edges.filterNot(contiguousCells.contains), soFar + 1)
    }

  private def getContiguousCells(start: Coord, toCheck: List[Coord]): List[Coord] =
    @tailrec
    def go(found: List[Coord], toExplore: List[Coord], current: Coord, i: Int): List[Coord] =
      val additionalFound = current.neighboursWithoutDiagonals.filter(neighbour => toCheck.contains(neighbour) && !found.contains(neighbour))
      val newToExplore = toExplore ++ additionalFound
      if (newToExplore.isEmpty) {
        found ++ additionalFound
      } else {
        go(found ++ additionalFound, newToExplore.tail, newToExplore.head, i + 1)
      }
    go(List(start), List(), start, 0)

  @tailrec
  private def bucketFill(l: List[String], found: Set[Coord], next: List[Coord], char: Char): Set[Coord] =
    if (next.isEmpty) {
      found
    } else {
      val nextCoord = next.head
      val matchingNonFoundNeighbours =
        getNeighbourCoordNoDiagonals(nextCoord.x, nextCoord.y, l)
          .map{case (x, y) => Coord(x, y)}
          .filter(c => l(c.y)(c.x) == char)
          .filterNot(c => found.contains(c))
      bucketFill(l, found ++ matchingNonFoundNeighbours.toSet, next.filterNot(_ == nextCoord) ++ matchingNonFoundNeighbours, char)
    }

  override def partTwo(l: List[String]): Long =
    val regions = getRegions(l.allCoords.toList, List(), l)
    regions.map(r => r.size.toLong * countContinguousEdges(allEdges(r), 0)).sum
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