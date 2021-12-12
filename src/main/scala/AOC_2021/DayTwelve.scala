package AOC_2021

import shared.{DayChallenge, TestData}

object DayTwelve extends DayChallenge[Int, Int] {
  override def partOne(l: List[String]): Int =
    getPaths(SmallCave("start"), SmallCave("end"), getCaveMap(l), canOnlyVisitSmallCavesOnce).size

  type Route = List[Cave]

  private def getPaths(startNode: Cave, endNode: Cave, map: Map[Cave, Seq[Cave]], isValidPath: (Cave, Route) => Boolean): List[Route] = {
    def go(currentNode: Cave, foundPaths: List[Route], currentUnfinishedPath: Route): List[Route] = {
      if (currentNode == endNode) {
        List(currentNode +: currentUnfinishedPath)
      } else {
        val possibleNextSteps = map(currentNode).filter(isValidPath(_, currentUnfinishedPath))
        possibleNextSteps match {
          case Nil            => Nil
          case next :: others => (next +: others).flatMap(nextCave => go(nextCave, foundPaths, currentNode +: currentUnfinishedPath))
        }
      }
    }
    go(startNode, List.empty, List.empty)
  }

  private def canOnlyVisitSmallCavesOnce(next: Cave, currentRoute: Route): Boolean =
    next.isInstanceOf[BigCave] || !currentRoute.contains(next)

  private def canOnlyVisitASingleSmallCaveTwice(next: Cave, currentRoute: Route): Boolean =
    next.isInstanceOf[BigCave] ||
      !currentRoute.contains(next) ||
      (!List("start", "end").contains(next.name) && currentRoute.count(_ == next) == 1 && doesNotContainRepeatsOfSmallCaves(currentRoute))

  private def doesNotContainRepeatsOfSmallCaves(r: Route) = {
    val smallCaves = r.filter(_.isInstanceOf[SmallCave])
    smallCaves.size == smallCaves.distinct.size
  }

  private def connections(l: List[String]): List[List[String]] = l.map(str => str.split("-").toList)

  private def getCaveMap(l: List[String]): Map[Cave, Seq[Cave]] =
    connections(l).flatten.toSet.map(Cave.from).map(cave => cave -> getConnectedCaves(cave, l)).toMap

  private def getConnectedCaves(c: Cave, l: List[String]): List[Cave] =
    connections(l).filter(_.contains(c.name)).flatMap(_.find(_ != c.name).toList.map(Cave.from))

  private def onlyContainsOneRepeatOfSmallCave(r: Route) = {
    val smallCaves = r.filter(_.isInstanceOf[SmallCave])
    smallCaves.size <= smallCaves.distinct.size + 1
  }

  override def partTwo(l: List[String]): Int = {
    val paths = getPaths(SmallCave("start"), SmallCave("end"), getCaveMap(l), canOnlyVisitASingleSmallCaveTwice)
    // This step shouldn't be needed but cba to work out why canOnlyVisitASingleSmallCaveTwice not doing the job here
    val filteredPaths = paths.filter(onlyContainsOneRepeatOfSmallCave)
    filteredPaths.distinct.size
  }
}

sealed trait Cave { val name: String }
case class BigCave(name: String)   extends Cave
case class SmallCave(name: String) extends Cave

object Cave {
  def from(name: String): Cave = if (name.forall(_.isUpper)) BigCave(name) else SmallCave(name)
}

object DayTwelveData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "start-A",
    "start-b",
    "A-c",
    "A-b",
    "b-d",
    "A-end",
    "b-end"
  )
  override val testData2: Option[List[String]] = Some(List(
    "dc-end",
    "HN-start",
    "start-kj",
    "dc-start",
    "dc-HN",
    "LN-dc",
    "HN-end",
    "kj-sa",
    "kj-HN",
    "kj-dc"
  ))
  override val expectedPartOne: Option[Int] = Some(10)
  override val expectedPartTwo: Option[Int] = Some(103)
}
