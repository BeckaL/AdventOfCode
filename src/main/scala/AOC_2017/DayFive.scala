package AOC_2017

import shared.{DayChallenge, TestData}

import scala.annotation.tailrec

object DayFive extends DayChallenge[Int, Int]{
  override def partOne(l: List[String]): Int =
    traverseMaze(m = makeMap(l), elemUpdater = int => int + 1)

  override def partTwo(l: List[String]): Int =
    traverseMaze(m = makeMap(l), elemUpdater = int => if (int > 2) int - 1 else int + 1)

  @tailrec
  private def traverseMaze(m: Map[Int, Int], elemUpdater: Int => Int, currentI: Int = 0, numberOfSteps: Int = 0): Int = {
    m.get(currentI) match {
      case None => numberOfSteps
      case Some(jump) =>
        traverseMaze(m.updated(currentI, elemUpdater(jump)), elemUpdater, currentI + jump, numberOfSteps + 1)
    }
  }

  private def makeMap(l: List[String]): Map[Int, Int] =
    l.map(_.toInt).zipWithIndex.map{case (k, index) => index -> k}.toMap
}

object DayFiveData extends TestData[Int, Int] {
  override val testData: List[String] = "0\n3\n0\n1\n-3".split("\n").toList
  override val expectedPartOne: Option[Int] = Some(5)
  override val expectedPartTwo: Option[Int] = Some(10)
}
