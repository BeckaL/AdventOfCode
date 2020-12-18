package AOC_2020

import org.scalatest.FlatSpec
import shared.{DayChallenge, DayChallengeBehaviours}

class DayFourteenTest extends FlatSpec with DayChallengeBehaviours[Int, Int] {
  override val day = "Day fourteen"
  override val file: String = "Day14Input"
  override val dayChallenge: DayChallenge[Int,Int] = DayFourteen

  private val testInput = dayChallenge.testData

  day should behave like partOneWorkingCorrectly(testInput, dayChallenge.expectedPartOne.get)

  it should behave like partOneWorksOnRealData(0)

  it should behave like partTwoWorksOnTestData(testInput, dayChallenge.expectedPartTwo.getOrElse(0))

  it should behave like partTwoWorksOnRealData(0)
}
