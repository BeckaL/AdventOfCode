package AOC_2017

import shared.{DayChallenge, Helpers, TestData}

object DaySeven extends DayChallenge[String, Int] with Helpers {
  override def partOne(l: List[String]): String =
    getBottomElement(getTower(l))

  private def getBottomElement(tower: Map[String, TowerElement]): String =
    tower.find{case (key, _) => !tower.values.toList.flatMap(_.programs).contains(key)}.get._1

  private def getTower(l: List[String]): Map[String, TowerElement] =
    l.map(getTwoFromSplit(_, " \\(")).map{case (name, rest) => name -> getSizeAndListOfPrograms(name, rest)}.toMap

  private def getSizeAndListOfPrograms(name: String, s: String): TowerElement = s.split(" -> ").toList match {
    case size :: Nil => TowerElement(name, size.replaceAll("\\)", "").toInt, Nil)
    case size :: programs :: Nil => TowerElement(name, size.replaceAll("\\)", "").toInt, programs.split(", ").toList)
    case _ => throw new RuntimeException("ahh don't know what happened there")
  }

  case class TowerElement(id: String, weight: Int, programs: List[String])

  //gah this whole thing is very ugly
  override def partTwo(l: List[String]): Int = {
    val tower = getTower(l)
    val bottomElem = getBottomElement(tower)
    val parentElem = findLowestImbalanceParent(tower, None, tower(bottomElem))
    val weightsOfParent = parentElem.programs.map(pId => getWeight(tower(pId), tower))
    val inbalancedChildWeight = weightsOfParent.find(e => weightsOfParent.count(_ == e) == 1).get
    val diff = weightsOfParent.find(e => weightsOfParent.count(_ == e) > 1).get - inbalancedChildWeight
    tower(parentElem.programs(weightsOfParent.indexOf(inbalancedChildWeight))).weight + diff
  }

  private def getWeight(elem: TowerElement, tower: Map[String, TowerElement]): Int = {
    elem.programs match {
      case Nil => elem.weight
      case otherPrograms => otherPrograms.map(programId => getWeight(tower(programId), tower)).sum + elem.weight
    }
  }

  private def findLowestImbalanceParent(tower: Map[String, TowerElement], parentElem: Option[TowerElement], currentElem: TowerElement): TowerElement = {
    if (currentElem.programs.map(programId => getWeight(tower(programId), tower)).toSet.size == 1) {
      parentElem.get
    } else {
      val weights = currentElem.programs.map(pId => getWeight(tower(pId), tower))
      val unbalancedIndex = weights.indexOf(weights.find(w => weights.count(_ == w) == 1).get)
      val unbalancedElemId = currentElem.programs(unbalancedIndex)
      findLowestImbalanceParent(tower, Some(currentElem), tower(unbalancedElemId))
    }
  }
}

object DaySevenData extends TestData[String, Int] {
  override val expectedPartOne: Option[String] = Some("tknk")
  override val testData: List[String] = "pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\nfwft (72) -> ktlj, cntj, xhth\nqoyq (66)\npadx (45) -> pbga, havc, qoyq\ntknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)".split("\n").toList
  override val expectedPartTwo: Option[Int] = Some(60)
}
