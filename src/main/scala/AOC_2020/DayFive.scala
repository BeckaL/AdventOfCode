package AOC_2020

import shared.DayChallenge

object DayFive extends DayChallenge[Int, Int] {
  override def partOne(l: List[String]): Int =
    l.map(getSeatId).max


  override def partTwo(l: List[String]): Int = {
    val seatIds = l.map(getSeatId).sorted
    seatIds.zipWithIndex.find { case (seatId, i) => seatIds(i + 1) == seatId + 2 }.get._1 + 1
  }


  private def getSeatId(str: String): Int = {
    val row = getElementByBinarySpacePartition(str.take(7), (0 to 127).toList, 'F', 'D')
    val col = getElementByBinarySpacePartition(str.takeRight(3), (0 to 7).toList, 'L', 'R')
    row * 8 + col
  }

  private def getElementByBinarySpacePartition(str: String, elems: List[Int], upChar: Char, downChar: Char): Int = {
    str.foldLeft(elems) { case (remainingElems, char) => {
      val split = remainingElems.splitAt(remainingElems.size / 2)
      if (char == upChar) split._1 else split._2
    }}.head
  }
}
