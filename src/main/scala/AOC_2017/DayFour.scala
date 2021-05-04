package AOC_2017

import shared.DayChallenge

object DayFour extends DayChallenge[Int, Int] {
  override def partOne(l: List[String]): Int = l.map(_.split(" ").toList).count(containsNoDuplicates)

  private def containsNoDuplicates(passphrase: List[String]) = passphrase.toSet.size == passphrase.size

  override val testData: List[String] = "aa bb cc dd ee\naa bb cc dd aa\naa bb cc dd aaa".split("\n").toList
  override val expectedPartOne: Option[Int] = Some(2)

  override def partTwo(l: List[String]): Int = l.map(_.split(" ").toList).count(containsNoAnagrams)

  private def containsNoAnagrams(passphrase: List[String]) =
    containsNoDuplicates(passphrase.map(string => string.sorted))

  override val testData2: Option[List[String]] = Some("abcde fghij\nabcde xyz ecdab\na ab abc abd abf abj\niiii oiii ooii oooi oooo\noiii ioii iioi iiio".split("\n").toList)
  override val expectedPartTwo: Option[Int] = Some(3)
}
