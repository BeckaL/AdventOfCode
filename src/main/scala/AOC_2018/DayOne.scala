package AOC_2018

import shared.{DayChallenge, Helpers}

object DayOne extends DayChallenge[Int, Int] with Helpers {
  def partOne(input: List[String]): Int =
    input.map(interpretNumberWithSign).sum

  override def partTwo(input: List[String]): Int = {
    def go(i: Int, previouslySeen: Set[Int], current: Int): Int = {
      val newFreq = current + interpretNumberWithSign(input(i % input.size ))
      if (previouslySeen contains newFreq) newFreq
        else go(i + 1, previouslySeen + newFreq, newFreq)
      }

    go(0, Set(0), 0)
  }

}
