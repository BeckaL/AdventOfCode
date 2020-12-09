package AOC_2020

import org.scalatest.FlatSpec
import shared.{DayChallenge, DayChallengeBehaviours}

class DayTenTest extends FlatSpec with DayChallengeBehaviours[Int, Int] {
  override val day = "Day ten"
  override val file: String = "Day10Input"
  override val dayChallenge: DayChallenge[Int, Int] = DayTen

  private val testInput = dayChallenge.testData

  day should behave like partOneWorkingCorrectly(testInput, dayChallenge.expectedPartOne.get)

  it should behave like partOneWorksOnRealData(0)

  it should behave like partTwoWorksOnTestData(testInput, dayChallenge.expectedPartTwo.getOrElse(0))

  it should behave like partTwoWorksOnRealData(0)
}
