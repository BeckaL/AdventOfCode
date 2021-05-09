package AOC_2017

import shared.{DayChallenge, Helpers}

object DayTwelve extends DayChallenge[Int, Int] with Helpers {

  override def partOne(l: List[String]): Int =
    getGroupI(0, l.map(getPipeAndConnectors).toMap)._2.size

  type PipesAndConnectors = Map[Int, Set[Int]]

  private def getGroupI(i: Int, m: PipesAndConnectors): (PipesAndConnectors, Set[Int]) = {
    def reduceGroup(ungrouped: PipesAndConnectors, grouped: Set[Int]): (PipesAndConnectors, Set[Int]) = {
      val toRemove = ungrouped.filter { case (pipe, connectingPipe) => grouped.intersect(connectingPipe + pipe).nonEmpty }
      val remainingUngrouped = ungrouped.filterNot { case (pipe, _) => toRemove.keys.toList contains pipe }.toMap
      val additionalInGroup = toRemove.map { case (pipe, connectingPipes) => connectingPipes + pipe }.toSet.flatten
      if (additionalInGroup.isEmpty) {
        (remainingUngrouped, grouped)
      } else {
        reduceGroup(remainingUngrouped, grouped ++ additionalInGroup)
      }
    }
    val groupSet = m(i) + i
    reduceGroup(m.filterNot{case(pipe, _) => pipe == 0}, groupSet)
  }

  override def partTwo(l: List[String]): Int = groupAll(l.map(getPipeAndConnectors).toMap)

  private def groupAll(ungrouped: PipesAndConnectors, groupsFound: Int = 0): Int =
    if (ungrouped.isEmpty) {
      groupsFound
    } else {
      groupAll(getGroupI(ungrouped.keys.head, ungrouped)._1, groupsFound + 1)
    }

  private def getPipeAndConnectors(s: String): (Int, Set[Int]) = {
    val (pipeString, connectorsString) = getTwoFromSplit(s, " <-> ")
    pipeString.toInt -> connectorsString.split(", ").toList.map(_.toInt).toSet
  }

  override val testData: List[String] =
    List(
      "0 <-> 2",
      "1 <-> 1",
      "2 <-> 0, 3, 4",
      "3 <-> 2, 4",
      "4 <-> 2, 3, 6",
      "5 <-> 6",
      "6 <-> 4, 5"
    )

  override val expectedPartOne: Option[Int] = Some(6)
  override val expectedPartTwo: Option[Int] = Some(2)
}
