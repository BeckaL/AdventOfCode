package AOC_2025

import shared.{DayChallenge, Helpers, TestData}

import scala.annotation.tailrec

object DayFive extends DayChallenge[Int, Long] with Helpers {
  override def partOne(l: List[String]): Int =
    val parts = splitIntoGroupsOfList(l)
    val ranges = parseRanges(parts.head)
    val ingredients = parts.last.map(_.toLong)
    ingredients.count(id => ranges.exists { case (s, e) => id >= s && id <= e })

  override def partTwo(l: List[String]): Long =
    val ranges = parseRanges(splitIntoGroupsOfList(l).head)
    val mergedRanges = mergeRangesRecursive(ranges)
    mergedRanges.map { case (start, end) => (end + 1) - start }.sum

  private def parseRanges(l: List[String]): List[(Long, Long)] =
    l.map(getTwoFromSplit(_, "-")).map { case (x, y) => (x.toLong, y.toLong) }

  @tailrec
  private def mergeRangesRecursive(ranges: List[(Long, Long)]): List[(Long, Long)] =
    val rangesWithIndex = ranges.zipWithIndex
    rangesWithIndex.find { case (r1, i) =>
      rangesWithIndex.exists { case (r2, i2) => i2 != i && isOverlap(r1, r2) }
    } match {
      case None => ranges
      case Some(r1, i1) =>
        val toMerge = rangesWithIndex.find { case (r2, i2) => i2 != i1 && isOverlap(r1, r2) }.get
        val rangesWithoutMerged = rangesWithIndex.filter { case (_, i) => i != i1 && i != toMerge._2 }.map(_._1)
        val newMergedRange = mergeRanges(r1, toMerge._1)
        mergeRangesRecursive(rangesWithoutMerged :+ newMergedRange)
    }

  private def isOverlap(r1: (Long, Long), r2: (Long, Long)) =
    (r2._1 >= r1._1 && r2._1 <= r1._2) || (r1._2 >= r2._1 && r1._2 <= r2._2)

  private def mergeRanges(r1: (Long, Long), r2: (Long, Long)) =
    (List(r1._1, r2._1).min, List(r1._2, r2._2).max)
}

object DayFiveData extends TestData[Int, Long] {
  override val testData: List[String] = List(
    "3-5",
    "10-14",
    "16-20",
    "12-18",
    "",
    "1",
    "5",
    "8",
    "11",
    "17",
    "32"
  )
  override val expectedPartOne: Option[Int] = Some(3)
  override val expectedPartTwo: Option[Long] = Some(14)
}