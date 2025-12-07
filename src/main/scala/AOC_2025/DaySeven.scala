package AOC_2025

import shared.{Coord, DayChallenge, GridHelpers, TestData}

import scala.annotation.tailrec

object DaySeven extends DayChallenge[Int, Long] with GridHelpers{
  override def partOne(l: List[String]): Int =
    val splitters = getSplitters(l)
    val maxY = l.indices.max
    val firstSplitter = splitters.find(c => c.y == 0).get
    traverseGridFindingSplittersHit(splitters, Map(firstSplitter -> 1), l.indices.max, 1)

  override def partTwo(l: List[String]): Long =
    val splitters = getSplitters(l)
    val firstSplitter = splitters.find(c => c.y == 0).get
    traverseGrid(splitters, Map(firstSplitter -> 1), l.indices.max).values.sum

  @tailrec
  private def traverseGrid(splitters: Set[Coord], coordHitFrequencies: Map[Coord, Long], maxY: Int): Map[Coord, Long] =
    if (coordHitFrequencies.keySet.head.y == maxY)
      coordHitFrequencies
    else
      val newF = exploreLayer(splitters, coordHitFrequencies)
      traverseGrid(splitters, newF, maxY)

  @tailrec
  private def traverseGridFindingSplittersHit(splitters: Set[Coord], f: Map[Coord, Long], maxY: Int, splittersHit: Int): Int =
    if (f.keySet.head.y == maxY)
      splittersHit
    else
      val newF = exploreLayer(splitters, f)
      val newSplittersHit = newF.keySet.intersect(splitters)
      traverseGridFindingSplittersHit(splitters, newF, maxY, splittersHit + newSplittersHit.size)

  private def exploreLayer(splitters: Set[Coord], frequenciesOfThisLayer: Map[Coord, Long]): Map[Coord, Long]  =
    def go(remainingToExplore: List[Coord], frequencyMapOfNextLayer: Map[Coord, Long]): Map[Coord, Long] =
      remainingToExplore match
        case Nil => frequencyMapOfNextLayer
        case nextNode :: others =>
          val numberOfBeams = frequenciesOfThisLayer(nextNode)
          val newNodes = if (splitters.contains(nextNode))
            List(nextNode.move((-1, 1)), nextNode.move((1, 1)))
          else
            List(nextNode.move((0, 1)))
          val updatedFrequencyMap = newNodes.foldLeft(frequencyMapOfNextLayer) {case (currentMap, c) =>
            currentMap.updated(c, currentMap.getOrElse(c, 0L) + numberOfBeams)
          }
          go(others, updatedFrequencyMap)

    go(frequenciesOfThisLayer.keySet.toList, Map())

  private def getSplitters(l: List[String]): Set[Coord] =
    l.allCoords.filter(c => l(c.y)(c.x) != '.').toSet
}

object DaySevenData extends TestData[Int, Long] {
  override val testData: List[String] = List(
    ".......S.......",
    "...............",
    ".......^.......",
    "...............",
    "......^.^......",
    "...............",
    ".....^.^.^.....",
    "...............",
    "....^.^...^....",
    "...............",
    "...^.^...^.^...",
    "...............",
    "..^...^.....^..",
    "...............",
    ".^.^.^.^.^...^.",
    "..............."
  )
  override val expectedPartOne: Option[Int] = Some(21)
  override val expectedPartTwo: Option[Long] = Some(40)
}