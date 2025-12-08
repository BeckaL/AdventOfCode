package AOC_2025

import shared.{Coord3D, DayChallenge, TestData}

import scala.annotation.tailrec

object DayEight extends DayChallenge[Int, Long] {
  override def partOne(l: List[String]): Int = 
    val coords = l.map(_.split(",").toList.map(_.toInt)).map(Coord3D.from)
    val n = if (l == DayEightData.testData) 10 else 1000
    val (d, allMap) = getAllClosestDistances(coords, 0, List(), Map())
    groupBeacons(d.take(n), allMap)

  private def groupBeacons(distances: List[Double], distancesToCoordPairs: Map[Double, (Coord3D, Coord3D)]): Int =
    @tailrec
    def go(groupings: Map[Coord3D, Int], groupsSoFar: Int, distancesLeftToConsider: List[Double]): (Map[Coord3D, Int], Int) =
      distancesLeftToConsider match {
        case Nil => (groupings, groupsSoFar)
        case firstDistance :: others =>
          val coordPair = distancesToCoordPairs(firstDistance)
          val (from, to) = (coordPair._1, coordPair._2)
          val (newGroupings, newGroupsSoFar) = (groupings.get(from), groupings.get(to)) match {
            case (None, None) =>
              (groupings.updated(from, groupsSoFar).updated(to, groupsSoFar), groupsSoFar + 1)
            case (Some(groupForFirstCoord), None) =>
              (groupings.updated(to, groupForFirstCoord), groupsSoFar)
            case (None, Some(groupForSecondCoord)) =>
              (groupings.updated(from, groupForSecondCoord), groupsSoFar)
            case (Some(firstCGroup), Some(secondCGroup)) =>
              if (firstCGroup == secondCGroup) {
                (groupings, groupsSoFar)
              } else {
                val updatedGroupingsWithMerge = groupings.filter((_, groupIndex) => groupIndex == firstCGroup || groupIndex == secondCGroup).foldLeft(groupings) {
                  case (currentGroupings, (coordToUpdate, _)) =>
                    currentGroupings.updated(coordToUpdate, groupsSoFar)
                }
                (updatedGroupingsWithMerge, groupsSoFar + 1)
              }
          }
          go(newGroupings, newGroupsSoFar, others)
      }

    val (finalGroupings, maxGroupIndex) = go(Map(), 0, distances)
    (0 to maxGroupIndex).map(i => i -> finalGroupings.count(_._2 == i)).map(_._2).sorted.reverse.take(3).product

  @tailrec
  private def getAllClosestDistances(coords: List[Coord3D], i: Int, distances: List[Double], distancesToCoordPairs: Map[Double, (Coord3D, Coord3D)]): (List[Double], Map[Double, (Coord3D, Coord3D)]) =
    if (i == coords.indices.max) {
      (distances.sorted, distancesToCoordPairs)
    } else {
      val fromC = coords(i)
      val toCompare = coords.drop(i + 1)
      val tosToDistances = toCompare.map(toCoord => toCoord -> straightLineDistance(fromC, toCoord))
      val updatedDistances = distances ++ tosToDistances.map(_._2)
      val updatedMap = distancesToCoordPairs ++ tosToDistances.map((to, dist) => dist -> (fromC, to))
      getAllClosestDistances(coords, i + 1, updatedDistances, updatedMap)
    }

  private def connectBox(c1: Coord3D, c2: Coord3D, coordsToGroups: Map[Coord3D, Int], nextI: Int, groupsSoFar: Int): (Map[Coord3D, Int], Int, Int) = {
    (coordsToGroups.get(c1), coordsToGroups.get(c2)) match {
      case (None, None) =>
        val newCoordsToGroups = coordsToGroups.updated(c1, nextI).updated(c2, nextI)
        (newCoordsToGroups, groupsSoFar + 1, nextI + 1)
      case (Some(firstCoordGroup), None) =>
        val newCoordsToGroups = coordsToGroups.updated(c2, firstCoordGroup)
        (newCoordsToGroups, groupsSoFar, nextI)
      case (None, Some(secondCoordGroup)) =>
        val newCoordsToGroups = coordsToGroups.updated(c1, secondCoordGroup)
        (newCoordsToGroups, groupsSoFar, nextI)
      case (Some(firstCoordGroup), Some(secondCoordGroup)) =>
        if (firstCoordGroup == secondCoordGroup) {
          (coordsToGroups, groupsSoFar, nextI)
        } else {
          val newCoordsToGroups = mergeCoordGroupings(firstCoordGroup, secondCoordGroup, coordsToGroups, nextI)
          (newCoordsToGroups, groupsSoFar - 1, nextI + 1)
        }
    }
  }

  private def mergeCoordGroupings(group1: Int, group2: Int, groupings: Map[Coord3D, Int], nextI: Int) =
    val toUpdate = groupings.filter((_, existingGroup) => existingGroup == group1 || existingGroup == group2)
    toUpdate.foldLeft(groupings) { case (newGroupings, (coordToAddToMergedGroup, _)) =>
      newGroupings.updated(coordToAddToMergedGroup, nextI)
    }

  private def connectBoxes(sortedDistances: List[Double], distancesToCoordPairs: Map[Double, (Coord3D, Coord3D)], totalCoords: Int): (Coord3D, Coord3D) = {
    @tailrec
    def go(groups: Map[Coord3D, Int], groupsSoFar: Int, nextIndex: Int, distancesRemaining: List[Double]): (Coord3D, Coord3D) = {
      distancesRemaining match {
        case Nil => throw new RuntimeException("didn't expect that")
        case nextClosest :: others =>
          val (c1, c2) = distancesToCoordPairs(nextClosest)
          val (newGroups, newGroupsSoFar, newNextIndex) = connectBox(c1, c2, groups, nextIndex, groupsSoFar)
          if (newGroups.size == totalCoords && newGroupsSoFar == 1) {
            (c1, c2)
          } else {
            go(newGroups, newGroupsSoFar, newNextIndex, others)
          }
      }
    }

    go(Map(), 0, 0, sortedDistances)
  }


  private def straightLineDistance(from: Coord3D, to: Coord3D) =
    Math.sqrt(
      Math.pow(from.x.toLong - to.x.toLong, 2) +
        Math.pow(from.y.toLong - to.y.toLong, 2) +
        Math.pow(from.z.toLong - to.z.toLong, 2))

  override def partTwo(l: List[String]): Long =
    val coords = l.map(_.split(",").toList.map(_.toInt)).map(Coord3D.from)
    val (d, allMap) = getAllClosestDistances(coords, 0, List(), Map())
    val (c1, c2) = connectBoxes(d, allMap, l.size)
    c1.x.toLong * c2.x.toLong
}

object DayEightData extends TestData[Int, Long] {
  override val testData: List[String] = List(
    "162,817,812",
    "57,618,57",
    "906,360,560",
    "592,479,940",
    "352,342,300",
    "466,668,158",
    "542,29,236",
    "431,825,988",
    "739,650,466",
    "52,470,668",
    "216,146,977",
    "819,987,18",
    "117,168,530",
    "805,96,715",
    "346,949,466",
    "970,615,88",
    "941,993,340",
    "862,61,35",
    "984,92,344",
    "425,690,689"
  )
  override val expectedPartOne: Option[Int] = Some(40)
  override val expectedPartTwo: Option[Long] = Some(25272)
}