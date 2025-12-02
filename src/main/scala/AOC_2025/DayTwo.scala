package AOC_2025

import shared.{DayChallenge, TestData}
import shared.Helpers

import scala.annotation.tailrec

object DayTwo extends DayChallenge[Long, Long] with Helpers {
  override def partOne(l: List[String]): Long =
    parseRanges(l).flatMap{ case (start, end) =>
      findInvalidInRange(start, end, repeatsOnce)
    }.sum

  override def partTwo(l: List[String]): Long =
    parseRanges(l).flatMap { case (start, end) =>
      findInvalidInRange(start, end, repeatsAnyNumberOfTimes)
    }.sum

  @tailrec
  private def findInvalidInRange(start: Long, end: Long, invalidFunction: String => Boolean, soFar: List[Long] = List()): List[Long] =
    if (start > end)
      soFar
    else
      val newSoFar = if (invalidFunction(start.toString)) start +: soFar else soFar
      findInvalidInRange(start + 1, end, invalidFunction, newSoFar)

  private def repeatsOnce(s: String): Boolean =
    val toTest = s.take(s.length / 2)
    s.length % 2 == 0 && toTest * 2 == s

  private def repeatsAnyNumberOfTimes(s: String): Boolean =
    var i = 1
    while (i - 1 < s.length / 2)
      if (s.length % i == 0) {
        val toTest = s.take(i)
        if ((toTest * (s.length / i)) == s) {
          return true
        }
      }
      i += 1
    false

  private def parseRanges(l: List[String]): List[(Long, Long)] =
    l.head.split(",").toList.map { s =>
      val (first, second) = getTwoFromSplit(s, "-")
      (first.toLong, second.toLong)
    }
}

object DayTwoData extends TestData[Long, Long] {
  override val testData: List[String] = List(
    "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
  )
  override val expectedPartOne: Option[Long] = Some(1227775554L)
  override val expectedPartTwo: Option[Long] = Some(4174379265L)
}