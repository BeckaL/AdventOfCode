package AOC_2024

import shared.{DayChallenge, Helpers, TestData}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.Map

object DayEleven extends DayChallenge[Long, Long] with Helpers {
  override def partOne(l: List[String]): Long = countStones(getCountMap(l.head), 25).toInt

  override def partTwo(l: List[String]): Long = countStones(getCountMap(l.head), 75)

  private def getCountMap(s: String) =
    val countMap: mutable.Map[Long, Long] = mutable.Map()
    extractLongs(s).foreach(i => countMap(i) = countMap.getOrElse(i, 0L) + 1)
    countMap

  @tailrec
  private def countStones(initial: mutable.Map[Long, Long], iterationsRemaining: Int): Long =
    if (iterationsRemaining == 0)
      initial.values.sum
    else
      val newMap: mutable.Map[Long, Long] = Map()
      initial.toList.foreach {case (stone, count) =>
        val result = transform(stone)
        result.foreach(i => newMap(i) = newMap.getOrElse(i, 0L) + count)
      }
      countStones(newMap, iterationsRemaining - 1)

  private def transform(stone: Long)=
    stone match
      case 0 => List(1L)
      case i if i.toString.length % 2 == 0 =>
        val split = i.toString.splitAt(i.toString.length / 2)
        List(split._1.toLong, split._2.toLong)
      case otherLong => List(otherLong * 2024)
}

object DayElevenData extends TestData[Long, Long] {
  override val testData: List[String] = List(
    "125 17"
  )
  override val expectedPartOne: Option[Long] = Some(55312)
  override val expectedPartTwo: Option[Long] = None
}