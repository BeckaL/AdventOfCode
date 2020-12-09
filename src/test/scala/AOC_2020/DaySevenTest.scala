package AOC_2020

import org.scalatest.FlatSpec
import shared.{DayChallenge, DayChallengeBehaviours}

class DaySevenTest extends FlatSpec with DayChallengeBehaviours[Int, Int] {
  override val day = "Day seven"
  override val file: String = "Day7Input"
  override val dayChallenge: DayChallenge[Int, Int] = DaySeven

  private val testInput = dayChallenge.testData

  day should behave like partOneWorkingCorrectly(testInput, dayChallenge.expectedPartOne.get)

  it should behave like partOneWorksOnRealData(0)

  it should behave like partTwoWorksOnTestData(testInput, dayChallenge.expectedPartTwo.getOrElse(100000))

  it should behave like partTwoWorksOnRealData(0)
}
