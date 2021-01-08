package AOC_2018

import org.scalatest.FlatSpec
import shared.{DayChallenge, DayChallengeBehaviours}

class DayTwentyThreeTest extends FlatSpec with DayChallengeBehaviours[Int,Int] {
  override val day = "Day 23"
  override val file: String = "Day23Input"
  override val year = 2018
  override val dayChallenge: DayChallenge[Int, Int] = DayTwentyThree

  private val testInput = dayChallenge.testData
    day should behave like partOneWorkingCorrectly(testInput, dayChallenge.expectedPartOne.get)
    it should behave like partOneWorksOnRealData(721)

  val testInput2 = List(
      "pos=<10,12,12>, r=2",
      "pos=<12,14,12>, r=2",
      "pos=<16,12,12>, r=4",
      "pos=<14,14,14>, r=6",
      "pos=<50,50,50>, r=200",
      "pos=<10,10,10>, r=5"
  )
    it should behave like partTwoWorksOnTestData(testInput2, dayChallenge.expectedPartTwo.get)
}
