package AOC_2017

import shared.{DayChallenge, TestData}

object DayFifteen extends DayChallenge[Int, Int] {
  private val aMultiplicationFactor = 16807
  private val bMultiplicationFactor = 48271
  private val divider: Long = 2147483647

  override def partOne(l: List[String]): Int = {
    val noOfIterations: Long = 40000000

    val (aStart, bStart) = getStartingValues(l)

    def duel(lastA: Long, lastB: Long, numberOfMatchesSoFar: Int, i: Long): Int =
      if (i == noOfIterations) numberOfMatchesSoFar else {
        val newA = (lastA * aMultiplicationFactor) % divider
        val newB = (lastB * bMultiplicationFactor) % divider
        val newNumberOfMatches = if (last16BitsMatch(newA, newB)) numberOfMatchesSoFar + 1 else numberOfMatchesSoFar
        duel(newA, newB, newNumberOfMatches, i + 1)
      }

    duel(aStart, bStart, 0, 0)
  }

  private def last16BitsMatch(a: Long, b: Long): Boolean =
    a.toBinaryString.takeRight(16) == b.toBinaryString.takeRight(16)

  private def getStartingValues(l: List[String]): (Long, Long) = {
    val seeds = l.map(_.split(" ").last.toLong)
    (seeds.head, seeds(1))
  }

  private def getNumberOfMatches(reversedAs: List[Long], reversedBs: List[Long], minSize: Int): Int = {
    val trimmedAs = reversedAs.drop(reversedAs.size - minSize)
    val trimmedBs = reversedBs.drop(reversedBs.size - minSize)

    trimmedAs.zip(trimmedBs).count{ case (a, b) => last16BitsMatch(a, b)}
  }

  def getLast16BinaryCharacters(i: Long): String = i.toBinaryString.takeRight(16)

  override def partTwo(l: List[String]): Int = {
    val noOfIterations: Long = 5000000
    val (aStart, bStart) = getStartingValues(l)
    val onlyShowAIfResultIsDivisibleBy = 4
    val onlyShowBIfResultIsDivisibleBy = 8

    def duel(lastA: Long, lastB: Long, reversedAs: List[Long], reversedBs: List[Long], countOfAs: Int, countOfBs: Int): Int = {
      if (countOfAs >= noOfIterations && countOfBs >= noOfIterations) getNumberOfMatches(reversedAs, reversedBs, noOfIterations.toInt) else {
        val newA = (lastA * aMultiplicationFactor) % divider
        val newB = (lastB * bMultiplicationFactor) % divider
        val (newUnmatchedAs, newAs) = if (newA % onlyShowAIfResultIsDivisibleBy == 0) {
          (newA +: reversedAs, countOfAs + 1)
        } else (reversedAs, countOfAs)
        val (newUnmatchedBs, newBs) = if (newB % onlyShowBIfResultIsDivisibleBy == 0) {
          (newB +: reversedBs, countOfBs + 1)
        } else (reversedBs, countOfBs)

        duel(newA, newB, newUnmatchedAs, newUnmatchedBs, newAs, newBs)
      }
    }

    duel(aStart, bStart, Nil, Nil, 0, 0)
  }
}

object DayFifteenData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "Generator A starts with 65",
    "Generator B starts with 8921"
  )
  override val expectedPartOne: Option[Int] = Some(588)
  override val expectedPartTwo: Option[Int] = Some(309)
}
