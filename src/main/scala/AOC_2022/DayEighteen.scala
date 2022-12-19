package AOC_2022

import shared.{Coord3D, DayChallenge, Helpers, TestData}

object DayEighteen extends DayChallenge[Int, Int] with Helpers {
  override def partOne(l: List[String]): Int = insideVolume(l.map(line => Coord3D.from(extractInts(line))))

  private def insideVolume(squares: List[Coord3D]): Int =
    squares.map(s1 => 6 - squares.count (sharesSide(s1, _))).sum

  private def sharesSide(s1: Coord3D, s2: Coord3D): Boolean =
    (s1.x - s2.x).abs + (s1.y - s2.y).abs + (s1.z - s2.z).abs == 1

  private def fillCube(xs: Range, ys: Range, zs: Range, squares: Set[Coord3D]): Set[Coord3D] = {
    def getMapOfCoordsToFilled(map: Map[Coord3D, Boolean], next: List[Coord3D]): Map[Coord3D, Boolean] = {
      next match {
        case Nil => map
        case nextToVisit :: others =>
          val neighboursInScope = (nextToVisit +: nextToVisit.neighbours)
            .filterNot(map.keySet.contains)
            .filterNot(others.contains)
            .filter { case Coord3D(x, y, z) => xs.contains(x) && ys.contains(y) && zs.contains(z) }
          val updatedMap = neighboursInScope.foldLeft(map) { case (coordsToFilled, neighbour) =>
            coordsToFilled.updated(neighbour, !squares.contains(neighbour))
          }
          val nextToExplore = neighboursInScope.filterNot(squares.contains)
          getMapOfCoordsToFilled(updatedMap, others ++ nextToExplore)
      }
    }

    getMapOfCoordsToFilled(Map(), List(Coord3D(xs.min, ys.min, xs.min))).filter(_._2).keySet
  }

  override def partTwo(l: List[String]): Int = {
    val squares = l.map(line => Coord3D.from(extractInts(line)))
    val (xs, ys, zs) = (squares.map(_.x), squares.map(_.y), squares.map(_.z))
    val filledAir = fillCube(xs.min to xs.max, ys.min to ys.max, zs.min to zs.max, squares.toSet)
    val allCoords = for {
      x <- xs.min to xs.max
      y <- ys.min to ys.max
      z <- zs.min to zs.max
    } yield Coord3D(x, y, z)
    val insideCoords = allCoords.filterNot { case c => squares.contains(c) || filledAir.contains(c) }
    insideVolume(squares) - insideVolume(insideCoords.toList)
  }
}

object DayEighteenData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "2,2,2",
    "1,2,2",
    "3,2,2",
    "2,1,2",
    "2,3,2",
    "2,2,1",
    "2,2,3",
    "2,2,4",
    "2,2,6",
    "1,2,5",
    "3,2,5",
    "2,1,5",
    "2,3,5"
  )
  override val expectedPartOne: Option[Int] = Some(64)
  override val expectedPartTwo: Option[Int] = Some(58)
}