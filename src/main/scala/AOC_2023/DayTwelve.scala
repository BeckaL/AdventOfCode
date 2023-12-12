package AOC_2023

import shared.{DayChallenge, Helpers, TestData}
import shared.UpdaterHelpers.StringUpdater

object DayTwelve extends DayChallenge[Int, Int] with Helpers {
  override def partOne(l: List[String]): Int =
    l.map(line =>
      val (stringMatcher, numberString) = getTwoFromSplit(line, " ")
      countPossibilities(stringMatcher, extractInts(numberString))
    ).sum

  private def countPossibilities(s: String, numbers: List[Int]): Int = {
    val knownCount = s.count(_ == '#')
    val unknownCount = s.count(_ == '?')
    val numberOfUnknownThatMustBeHashes = numbers.sum - knownCount
    val charsToPlace = (0 until numberOfUnknownThatMustBeHashes).map(_ => '#').toList ++
      (0 until unknownCount - numberOfUnknownThatMustBeHashes).map(_ => '.').toList
    charsToPlace.permutations.map(replaceUnknownCharsInString(s, _))
      .count(stringMatchesNumbers(_, numbers))
  }

  private def replaceUnknownCharsInString(s: String, toReplace: List[Char]): String =
    toReplace match
      case Nil => s
      case firstCharToReplace :: others =>
        replaceUnknownCharsInString(s.updateChar(s.indexOf('?'), firstCharToReplace), others)

  private def stringMatchesNumbers(s: String, numbers: List[Int]): Boolean =
    """#+""".r.findAllIn(s).toList.map(_.length) == numbers

  override def partTwo(l: List[String]): Int = {
    2
  }
}

object DayTwelveData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "???.### 1,1,3",
      ".??..??...?##. 1,1,3",
      "?#?#?#?#?#?#?#? 1,3,1,6",
      "????.#...#... 4,1,1",
      "????.######..#####. 1,6,5",
      "?###???????? 3,2,1"
  )
  override val expectedPartOne: Option[Int] = Some(21)
  override val expectedPartTwo: Option[Int] = Some(0)
}