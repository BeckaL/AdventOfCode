package AOC_2020

import org.scalatest.FlatSpec
import shared.{DayChallenge, DayChallengeBehaviours}

class DayElevenTest extends FlatSpec with DayChallengeBehaviours[Int, Int] {
  override val day = "Day eleven"
  override val file: String = "Day11Input"
  override val dayChallenge: DayChallenge[Int,Int] = DayEleven

  private val testInput = dayChallenge.testData
  day should behave like partOneWorkingCorrectly(testInput, dayChallenge.expectedPartOne.get)

  it should behave like partOneWorksOnRealData(2368)

  it should behave like partTwoWorksOnTestData(testInput, dayChallenge.expectedPartTwo.getOrElse(0))

  it should behave like partTwoWorksOnRealData(2124)

}
