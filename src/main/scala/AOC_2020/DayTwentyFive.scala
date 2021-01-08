package AOC_2020

import shared.DayChallenge

object DayTwentyFive extends DayChallenge[Long, Long] {
  override val expectedPartOne: Option[Long] = Some(14897079)
  override val expectedPartTwo: Option[Long] = Some(0)
  override val testData: List[String] = List(
    "5764801",
    "17807724"
  )
  override def partOne(l: List[String]): Long = {
    val cardPublicKey = l.head.toLong
    val doorPublicKey = l(1).toLong
    val cardLoopSize = getLoopSize(cardPublicKey, 1,  1)
    val doorLoopSize = getLoopSize(doorPublicKey,  1, 1)
    transformWithLoopSize(cardLoopSize, doorPublicKey)
  }

  private def transformWithLoopSize(i:  Int,subjectNumber: Long):Long = {
    (1 to i).foldLeft(1.toLong)((value,_) => (value * subjectNumber) % 20201227)
  }

  private def getLoopSize(target: Long, i: Int, current: Long): Int = {
    val subjectNumber = 7
    val newCurrent = (current * subjectNumber) % 20201227
    if (newCurrent== target) {
      i
    } else {
      getLoopSize(target, i + 1, newCurrent)
    }

  }

  override def partTwo(l: List[String]): Long = ???
}
