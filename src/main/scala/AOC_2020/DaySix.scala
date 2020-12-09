package AOC_2020

import shared.{DayChallenge, Helpers}

object DaySix extends DayChallenge[Int, Int] with Helpers {
  override val testData: List[String] = List("abc",
    "",
    "a",
    "b",
    "c",
    "",
    "ab",
    "ac",
    "",
    "a",
    "a",
    "a",
    "a",
    "",
    "b")

  override val expectedPartOne: Option[Int] = Some(11)

  override def partOne(l: List[String]): Int =
    splitIntoGroupsOfString(l).map(_.replaceAll(" ", "").toList.distinct.size).sum

  override val expectedPartTwo: Option[Int] = Some(6)


  override def partTwo(l: List[String]): Int =
    splitIntoGroupsOfList(l).map(group => group.flatten.distinct.count(char => group.forall(p => p contains char))).sum
}
