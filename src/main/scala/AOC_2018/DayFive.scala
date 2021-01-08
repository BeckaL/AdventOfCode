package AOC_2018

import shared.DayChallenge

object DayFive extends DayChallenge[Int, Int] {
  override val expectedPartOne: Option[Int] = Some(10)
  override val expectedPartTwo: Option[Int] = Some(4)
  override val testData: List[String] = List("dabAcCaCBAcCcaDA")
  override def partOne(l: List[String]): Int = {
    go(l.head).length
  }
  def go(elem: String): String = {
    def go2(stringToIterate: String, currentString: String): String = {
      if (stringToIterate.size < 2) {
        currentString + stringToIterate
      } else if (isSameCharWithDifferentCase(stringToIterate(0), stringToIterate(1))) {
        go2(stringToIterate.takeRight(stringToIterate.size - 2),currentString)
      } else {
        go2(stringToIterate.takeRight(stringToIterate.size - 1), currentString + stringToIterate(0))
      }
    }
    val newS = go2(elem,"")
    if (newS.length ==  elem.length) {
      newS
    } else{
      go(newS)
    }
  }

  def isSameCharWithDifferentCase(c1: Char, c2: Char): Boolean = if(c1.isLower) {
    c1.toUpper == c2
  } else {
    c1.toLower == c2
  }
  override def partTwo(l: List[String]): Int = {
    val chars: Set[Char] = l.head.toSet
    val chars2: List[Char] = chars.map(_.toLower).toList.sorted
    chars2.map{lowerC =>
      val filtered = l.head.filterNot(c => c == lowerC || c == lowerC.toUpper).mkString
      println(s"c is $lowerC")
      go(filtered).size
    }.min
  }
}
