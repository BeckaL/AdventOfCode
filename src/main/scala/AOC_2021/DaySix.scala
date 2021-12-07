package AOC_2021

import shared.{DayChallenge, TestData}

object DaySix extends DayChallenge[Long, Long] {
  override def partOne(l: List[String]): Long =
    getFishMapAfterNIterations(start = getFishMap(l), iterations = 80).values.sum

  override def partTwo(l: List[String]): Long =
    getFishMapAfterNIterations(start = getFishMap(l), iterations = 256).values.sum

  private def getFishMapAfterNIterations(start: Map[Int, Long], iterations: Int) =
    (0 until iterations).foldLeft(start) { case (map, _) => getNewLanternfish(map) }

  private def getFishMap(l: List[String]): Map[Int, Long] =
    l.head.split(",").map(_.toInt).groupBy(identity).map { case (i, grouped) => i -> grouped.size.toLong }

  private def getNewLanternfish(f: Map[Int, Long]): Map[Int, Long] =
    (0 to 8).map {
      case 8 => 8 -> f.getOrElse(key = 0, default = 0L)
      case 6 => 6 -> (f.getOrElse(key = 0, default = 0L) + f.getOrElse(key = 7, default = 0L))
      case x => x -> f.getOrElse(key = x + 1, default = 0L)
    }.toMap
}

object DaySixData extends TestData[Int, Long] {
  override val testData: List[String]        = List("3,4,3,1,2")
  override val expectedPartOne: Option[Int]  = Some(5934)
  override val expectedPartTwo: Option[Long] = Some(26984457539L)
}
