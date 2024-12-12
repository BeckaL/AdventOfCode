package AOC_2024

import shared.{Coord, DayChallenge, GridHelpers, TestData}

import scala.annotation.tailrec

object DayTwelve extends DayChallenge[Long, Long] with GridHelpers {
  override def partOne(l: List[String]): Long = {
    val regions = getRegions(l.allCoords.toList, List(), l)
    regions.map(r => r.size.toLong * getPerimeter(r, l)).sum
  }

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

  private def countSides(r: Set[Coord], l: List[String]) =
    val topLeft = r.toList.filter(c => c.y == r.map(_.y).min).minBy(_.x)
    println(s"region is $r")
    val sides = traverseBoundary(topLeft, 'N', l, List(), r, 1)
    println(s"\nchar is ${l(topLeft.y)(topLeft.x)}")
    println(s"sides are $sides\n")
    sides

  @tailrec
  private def traverseBoundary(c: Coord, position: Char, l: List[String], traversedAlready: List[(Coord, Char)], r: Set[Coord], soFar: Int): Int =
    println(s"\ntraversing boundary from $c with edge $position")
    val neighbours = outsideCoordsWithNeighbouringSides(c, r, position, l, List(c), List(c))
    println(s"neighbours are $neighbours")
    val endCoord = position.match {
      case 'N' => neighbours.maxBy(_.x)
      case 'S' => neighbours.minBy(_.x)
      case 'E' => neighbours.maxBy(_.y)
      case _ => neighbours.minBy(_.y)
    }
    val (nextCoord, nextPosition) = position match {
      case 'N' =>
        val nextPosition = List((0, 1), (0, -1)).map{ case (xDiff, yDiif) => Coord(endCoord.x + xDiff, endCoord.y+yDiif)}.find(r.contains) match {
          case Some(c) => if (c.x < endCoord.x) 'W' else 'E'
          case None => 'E'
        }
        (endCoord, nextPosition)
      case 'S' =>
        val nextPosition =
          List((0, 1), (0, -1)).map{ case (xDiff, yDiif) => Coord(endCoord.x + xDiff, endCoord.y+yDiif)}.find(r.contains) match {
            case Some(c) => if(c.x < endCoord.y) 'W' else 'E'
            case None => 'W'
          }
        (endCoord, nextPosition)
      case 'E' =>
        List((1, 1), (-1, 0)).map{ case (xDiff, yDiif) => Coord(endCoord.x + xDiff, endCoord.y+yDiif)}.find(r.contains) match {
          case Some(c) =>
//            println(s"going east next is $c")
            if (c.y > endCoord.y) (c, 'N') else (endCoord, 'S')
          case None => (endCoord, 'S')
        }
      case _ => List((1, 0), (-1, 0)).map{ case (xDiff, yDiif) => Coord(endCoord.x + xDiff, endCoord.y+yDiif)}.find(r.contains) match {
        case Some(c) => if (c.y < endCoord.y) (endCoord, 'S') else (endCoord, 'N')
        case None => (endCoord, 'N')
      }
    }
//    println(nextCoord)
    if (traversedAlready.contains((nextCoord, nextPosition))) {
      soFar
    } else {
      traverseBoundary(nextCoord, nextPosition, l, (c, position) +: traversedAlready, r, soFar + 1)
    }

  @tailrec
  private def outsideCoordsWithNeighbouringSides(c: Coord, r: Set[Coord], position: Char, l: List[String], found: List[Coord], next: List[Coord]): List[Coord] =
    val vectors = if (Set('N', 'S').contains(position)) {
      List((-1, 0), (1, 0))
    } else {
      List((0, 1), (0, -1))
    }
    val relevantNeighbours = vectors
      .map{case (xDiff, yDiff) => Coord(c.x + xDiff, c.y + yDiff)}
      .filter(r.contains).filterNot(found.contains)
    val newFound = relevantNeighbours.filter(n =>
      position match {
        case 'N' => !r.contains(Coord(n.x, n.y - 1))
        case 'S' => !r.contains(Coord(n.x, n.y +1))
        case 'E' => !r.contains(Coord(n.x + 1, n.y))
        case _ => !r.contains(Coord(n.x - 1, n.y))
      })
    val newNext = newFound ++ next
    if (newNext.isEmpty) {
      found
    } else {
      outsideCoordsWithNeighbouringSides(newNext.head, r, position, l, found ++ newFound, newNext.tail)
    }


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

  override def partTwo(l: List[String]): Long = {
    val regions = getRegions(l.allCoords.toList, List(), l)
    regions.map(r => r.size.toLong * countSides(r, l)).sum

  }
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