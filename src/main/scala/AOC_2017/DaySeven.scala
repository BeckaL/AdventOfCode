package AOC_2017

import shared.{DayChallenge, Helpers}

object DaySeven extends DayChallenge[String, Int] with Helpers {
  override val expectedPartOne: Option[String] = Some("tknk")
  override val testData: List[String] = "pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\nfwft (72) -> ktlj, cntj, xhth\nqoyq (66)\npadx (45) -> pbga, havc, qoyq\ntknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)".split("\n").toList
  override def partOne(l: List[String]): String =
    getBottomElement(getTower(l))

  private def getBottomElement(tower: Map[String, (Int, List[String])]) = {
    val towerValues = tower.values.toList.flatMap(_._2)
    tower.find{case (key, _) => !towerValues.contains(key)}.get._1
  }

  private def getTower(l: List[String]): Map[String, (Int, List[String])] =
    l.map(getTwoFromSplit(_, " \\(")).map{case (name, rest) => name -> getSizeAndListOfPrograms(rest)}.toMap

  private def getSizeAndListOfPrograms(s: String): (Int, List[String]) = s.split(" -> ").toList match {
    case size :: Nil => (size.replaceAll("\\)", "").toInt, Nil)
    case size :: programs :: Nil => (size.replaceAll("\\)", "").toInt, programs.split(", ").toList)
    case _ => throw new RuntimeException("ahh don't know what happened there")
  }

  override def partTwo(l: List[String]): Int = ???
}
