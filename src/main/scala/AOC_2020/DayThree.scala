package AOC_2020

import shared.DayChallenge

object DayThree extends DayChallenge[Int, Int] {

  override def partOne(rows: List[String]): Int =
    countTreesOnPath(rows, 3, 1)

  override def partTwo(rows: List[String]): Int =
    List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
      .foldLeft(1){ case (current, (xInc, yInc)) => current * countTreesOnPath(rows, xInc, yInc)}

  private def countTreesOnPath(rows: List[String], xIncrement: Int, yIncrement: Int): Int =
    (0 until rows.length / yIncrement)
      .toList
      .map { i => rows(yIncrement * i)((xIncrement * i) % rows.head.length) }
      .count(_ == '#')
}
