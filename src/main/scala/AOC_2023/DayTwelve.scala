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

  def countPossibilitiesNew(s: String, numbers: List[Int]): Int = {
    println(s"counting possibilities with s $s and numbers $numbers")
    val knownCount = s.count(_ == '#')
    val unknownCount = s.count(_ == '?')
    val numberOfUnknownThatMustBeHashes = numbers.sum - knownCount
    if (numberOfUnknownThatMustBeHashes == unknownCount) {
      if (checkIfArrangementPossible(s, numbers)) 1 else 0
    } else if (numberOfUnknownThatMustBeHashes < unknownCount) {
      val firstUnknown = s.indexOf("?") //what if this is -1? Could it be with the above checks?
      println(firstUnknown)
      val firstKnown = s.indexOf("#")
      if (firstKnown >= 0 && firstKnown < firstUnknown) {
        if (checkIfPossible(s.substring(firstKnown), numbers)) {
          if (numbers.size == 1) {
            1
          } else {
            countPossibilitiesNew(s.substring(firstKnown + numbers.head + 1), numbers.tail)
          }
        } else 0
      } else {
        val replaced = s.updated(firstUnknown, '#')
        val possible = checkIfPossible(replaced.substring(firstUnknown), numbers)
        if (possible) {
//          println("is possible")
          if (numbers.size == 1)
            1 + countPossibilitiesNew(s.substring(firstUnknown + 1), numbers)
          else countPossibilitiesNew(s.substring(firstUnknown + numbers.head + 1), numbers.tail) + countPossibilitiesNew(s.substring(firstUnknown + 1), numbers)
        } else countPossibilitiesNew(s.tail, numbers)
      }
    } else 0
  }

  private def checkIfArrangementPossible(s: String, numbers: List[Int]) =
    stringMatchesNumbers(s.replaceAll("\\?", "#"), numbers)

  private def checkIfPossible(sFromFirstKnown: String, numbers: List[Int]) =
    val (nextNumber, others) = (numbers.head, if (numbers.size == 1) List() else numbers.tail)
    val slice = sFromFirstKnown.slice(0, nextNumber)
    val r = slice.length == nextNumber && slice.forall(char => char == '?' || char == '#') && (sFromFirstKnown.size == nextNumber || sFromFirstKnown(nextNumber) != '#')
//    println(s"possible is $r")
    r

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