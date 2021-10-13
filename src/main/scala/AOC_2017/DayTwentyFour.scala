package AOC_2017

import shared.{DayChallenge, TestData}
import shared.Helpers

object DayTwentyFour extends DayChallenge[Int, Int] with Helpers {
  override def partOne(l: List[String]): Int =
    findAllBridges(getConnectors(l)).map(getBridgeStrength).max

  override def partTwo(l: List[String]): Int = {
    val bridges = findAllBridges(getConnectors(l))
    val longestBridgeLength = bridges.maxBy(_.size).size
    bridges.filter(b => b.size == longestBridgeLength).map(getBridgeStrength).max
  }

  def findAllBridges(all: List[Connector]): List[List[Connector]] =
    all.filter(_.s == 0).flatMap { firstConnector =>
      findBridgesGivenStartConnector(all.filterNot(_ == firstConnector), List(List()), List(firstConnector))
    }

  def findBridgesGivenStartConnector(all: List[Connector], foundCompleteBridges: List[List[Connector]], current: List[Connector]): List[List[Connector]] = {
    val unusedPort = getUnusedPort(current)
    val possibleNexts = all.filter(b => b.s == unusedPort || b.e == unusedPort)
    possibleNexts match {
      case Nil => foundCompleteBridges :+ current
      case possibleNexts => possibleNexts.flatMap{ n =>
        val newAll = all.filterNot(_ == n)
        findBridgesGivenStartConnector(newAll, foundCompleteBridges, current :+ n)
      }
    }
  }

  def getBridgeStrength(bridge: List[Connector]) = bridge.map(connector => connector.s + connector.e).sum

  def getUnusedPort(current: List[Connector]) =
    current match {
      case onlyConnector :: Nil => onlyConnector.e
      case _ =>
        val previous = current(current.size - 2)
        if (current.last.s == previous.s || current.last.s == previous.e) current.last.e else current.last.s
    }

  def getConnectors(l: List[String]): List[Connector] =
    l.map{bridgeString => connectorFrom(getTwoFromSplit(bridgeString, "/"))}

  case class Connector(s: Int, e: Int)

  def connectorFrom(t: (String, String)) = Connector(t._1.toInt, t._2.toInt)
}

object DayTwentyFourData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "0/2",
    "2/2",
    "2/3",
    "3/4",
    "3/5",
    "0/1",
    "10/1",
    "9/10"
  )

  override val expectedPartOne: Option[Int] = Some(31)

  override val expectedPartTwo: Option[Int] = Some(19)
}
