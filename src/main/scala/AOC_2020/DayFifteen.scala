package AOC_2020

import shared.DayChallenge

import scala.annotation.tailrec

object DayFifteen extends DayChallenge[Long, Long] {
  override def partOne(l: List[String]): Long = getNthNumber(l.map(_.toInt), 2020)

  override def partTwo(l: List[String]): Long = getNthNumber(l.map(_.toInt), 30000000)

  def getNthNumber(starts: List[Int], n: Long): Long = {
    @tailrec
    def go(said: Map[Long, List[Long]], lastSaid: Long, goNumber: Long): Long =
    if (goNumber == n) {
      lastSaid
    } else {
      val newI = if (goNumber < starts.size) {
        starts(goNumber.toInt).toLong
      } else {
        said(lastSaid) match {
          case _ :: Nil => 0L
          case head :: tail => (head - tail.head)
          case _ => throw new RuntimeException("I didn't understand that")
        }
      }
      go(appendOrAdd(newI, goNumber, said), newI, goNumber + 1)
    }
    go(Map(), 0, 0)
  }

  def appendOrAdd(number: Long, turn: Long, said: Map[Long, List[Long]]): Map[Long, List[Long]] =
    said.get(number) match {
      case Some(list) => said + (number -> (turn +: list.take(1)))
      case None => said + (number -> List(turn))
    }

  override val expectedPartOne: Option[Long] = Some(436)
  override val expectedPartTwo: Option[Long] = Some(175594)
  override val testData: List[String] = List(0, 3, 6).map(_.toString)
}
