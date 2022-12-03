package AOC_2022

import shared.{DayChallenge, TestData}

object DayThree extends DayChallenge[Int, Int]{
  override def partOne(l: List[String]): Int =
    l.map { line =>
      val (first, second) = line.toList.splitAt(line.length / 2)
      priority(first.intersect(second).head)
    }.sum

  private def priority(c: Char): Int =
    if (c.isUpper) {
      c.toInt - 38
    } else {
      c.toInt - 96
    }

  override def partTwo(l: List[String]): Int =
    l.sliding(3, 3).map(group =>
      priority(group.head.intersect(group(1)).intersect(group(2)).head)
    ).sum

object DayThreeData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "vJrwpWtwJgWrhcsFMMfFFhFp",
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
    "PmmdzqPrVvPwwTWBwg",
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
    "ttgJtRGJQctTZtZT",
    "CrZsJsPPZsGzwwsLwLmpwMDw"
  )
  override val expectedPartOne: Option[Int] = Some(157)
  override val expectedPartTwo: Option[Int] = Some(70)
}

