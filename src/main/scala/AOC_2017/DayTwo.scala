package AOC_2017

import shared.DayChallenge

object DayTwo extends DayChallenge[Int, Int] {
  override def partOne(l: List[String]): Int =
    getRows(l).map(row => row.max - row.min).sum

  private def getRows(l: List[String]): List[List[Int]] =
    l.map(_.split("\\s").toList.map(_.toInt))

  override val testData: List[String] = List("5 1 9 5",
    "7 5 3",
    "2 4 6 8")
  override val expectedPartOne: Option[Int] = Some(18)

  override def partTwo(l: List[String]): Int =
    getRows(l).map(row =>
     getResultOfEvenlyDivisblePair(row.sorted)).sum

  def getResultOfEvenlyDivisblePair(rowAscending: List[Int]): Int =
    rowAscending.tail.find(k => k % rowAscending.head == 0) match {
      case Some(i) => i / rowAscending.head
      case None => getResultOfEvenlyDivisblePair(rowAscending.tail)
    }

  override val testData2: Option[List[String]] = Some("5 9 2 8\n9 4 7 3\n3 8 6 5".split("\n").toList)
  override val expectedPartTwo: Option[Int] = Some(9)
}
