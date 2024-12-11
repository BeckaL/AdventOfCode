package AOC_2024

import shared.{DayChallenge, Helpers, TestData}

import scala.annotation.tailrec

object DayEleven extends DayChallenge[Int, Int] with Helpers {
  override def partOne(l: List[String]): Int = {
    countStones(extractLongs(l.head), 75)
  }

  @tailrec
  private def countStones(initial: List[Long], iterationsRemaining: Int): Int =
//    println(initial)
    if (iterationsRemaining == 0) {
      initial.size
    } else {
      val newStones = initial.flatMap(stone =>
        stone match {
          case 0 => List(1L)
          case i if i.toString.length % 2 == 0 =>
            val split = i.toString.splitAt(i.toString.length / 2)
            List(split._1.toLong, split._2.toLong)
          case otherLong => List(otherLong * 2024)
        }
      )
      countStones(newStones, iterationsRemaining - 1)
    }

  override def partTwo(l: List[String]): Int = {
    2
  }
}

object DayElevenData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "125 17"
  )
  override val expectedPartOne: Option[Int] = Some(55312)
  override val expectedPartTwo: Option[Int] = None
}