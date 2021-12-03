package AOC_2021

import shared.{DayChallenge, TestData}

object DayThree extends DayChallenge[Int, Int] {
  override def partOne(l: List[String]): Int = {
    val mostCommonBits = l.head.indices.toList.map(mostCommonBit(l, _)).mkString
    val leastCommonBits = l.head.indices.toList.map(leastCommonBit(l, _)).mkString
    multiplyBinaryStrings(mostCommonBits, leastCommonBits)
  }

  def leastCommonBit(l: List[String], i: Int): Char =
    getCharsAtIWithFrequencies(l, i).minBy(_._2)._1

  def mostCommonBit(l: List[String], i: Int): Char =
    getCharsAtIWithFrequencies(l, i).maxBy(_._2)._1

  override def partTwo(l: List[String]): Int = {
    val oxygenRating= findBinaryStringMostMatching(l, '1', mostCommonBitWithTieBreaker)
    val scrubberRating = findBinaryStringMostMatching(l, '0', leastCommonBitWithTieBreaker)
    multiplyBinaryStrings(oxygenRating, scrubberRating)
  }

  def mostCommonBitWithTieBreaker(l: List[String], i: Int, tiebreaker: Char): Char = {
    val grouped = getCharsAtIWithFrequencies(l, i)
    if (grouped('0') == grouped('1'))
      tiebreaker
    else
      grouped.maxBy(_._2)._1
  }

  def getCharsAtIWithFrequencies(l: List[String], i: Int) =
    l.map(str => str.charAt(i)).groupBy(identity).map{case (char, chars) => (char, chars.size)}

  def leastCommonBitWithTieBreaker(l: List[String], i: Int, tiebreaker: Char): Char = {
    val grouped = getCharsAtIWithFrequencies(l, i)
    if (grouped('0') == grouped('1'))
      tiebreaker
    else
      grouped.minBy(_._2)._1
  }

  def findBinaryStringMostMatching(l: List[String], deciderIfEqual: Char, filterCondition: (List[String], Int, Char) => Char) = {
    def go(lToReduce: List[String], i: Int): String =
      lToReduce match {
        case head :: Nil => head
        case Nil => throw new RuntimeException("couldn't find anything")
        case _ =>
          val filterCharAtI = filterCondition(lToReduce, i, deciderIfEqual)
          val remaining = lToReduce.filter(str => str.charAt(i) == filterCharAtI)
          go(remaining, i + 1)
      }
    go(l, 0)
  }

  def multiplyBinaryStrings(a: String, b: String): Int =
    Integer.parseInt(a, 2) * Integer.parseInt(b, 2)
}

object DayThreeData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "00100",
  "11110",
  "10110",
  "10111",
  "10101",
  "01111",
  "00111",
  "11100",
  "10000",
  "11001",
  "00010",
  "01010"
  )
  override val expectedPartOne: Option[Int] = Some(198)
  override val expectedPartTwo: Option[Int] = Some(230)
}
