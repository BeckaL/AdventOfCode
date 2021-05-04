package AOC_2017

import shared.DayChallenge

object DayThree extends DayChallenge[Int, Int] {
  override val testData: List[String] = List("1024")
  override val expectedPartOne: Option[Int] = Some(31)

  override val testData2: Option[List[String]] = Some(List("800"))
  override val expectedPartTwo: Option[Int] = Some(806)

  override def partOne(l: List[String]): Int = getSpiralNumber(l.head.toInt)

  private def sumFactorial(i: Int) = if (i == 0) 0 else (1 to i).toList.sum

  private def getSpiralNumber(i: Int, currentSpiralNumber: Int = 1, maxFromPreviousSpiral: Int = 1): Int = {
    1 + 8 * sumFactorial(currentSpiralNumber) match {
      case maxForCurrentSpiral if maxForCurrentSpiral >= i =>
        val side = (currentSpiralNumber to currentSpiralNumber * 2 - 1).toList.reverse.tail ++ (currentSpiralNumber to currentSpiralNumber * 2).toList
        (side ++ side ++ side ++ side).zipWithIndex.find { case (distance, index) => maxFromPreviousSpiral + index + 1 == i }.map(_._1).get
      case _ =>
        getSpiralNumber(i, currentSpiralNumber + 1, 1 + 8 * sumFactorial(currentSpiralNumber))
    }
  }

  override def partTwo(l: List[String]): Int = ???
}
