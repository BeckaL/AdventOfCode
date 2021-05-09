package AOC_2017

import shared.DayChallenge

object DaySeventeen extends DayChallenge[Int, Int] {

  override def partOne(l: List[String]): Int = {
    val stepValue = l.head.toInt
    val (endBuffer, _) = (1 to 2017).foldLeft((Map(0 -> 0), 0)){case ((buffer, currentPosition), insertValue) =>
      (insertCircularBufferAfter(stepThrough(buffer, stepValue, currentPosition), buffer, insertValue), insertValue)
    }
    endBuffer(2017)
  }

  private def insertCircularBufferAfter(before: Int, m: Map[Int, Int], insertValue: Int): Map[Int, Int] =
    m.updated(before, insertValue).updated(insertValue, m(before))

  private def stepThrough(m: Map[Int, Int], steps: Int, currentValue: Int): Int =
    (0 until steps).foldLeft(currentValue){case (value, _) => m(value) }

  override def partTwo(l: List[String]): Int = {
      val stepValue = l.head.toInt
      (1 to 50000000).foldLeft((0, 0)) { case ((currentPosition, currentValueAfter0), insertValue) =>
        val newPosition = (currentPosition + stepValue) % insertValue + 1
        if (newPosition == 1) (newPosition, insertValue) else (newPosition, currentValueAfter0)
      }._2
  }

  override val expectedPartOne: Option[Int] = Some(638)
  override val expectedPartTwo: Option[Int] = Some(1222153)
  override val testData: List[String] = List("3")
}
