package AOC_2020

import org.scalatest.flatspec.AnyFlatSpec
import shared.{DayChallenge, DayChallengeBehaviours}

class DayTenTest extends AnyFlatSpec with DayChallengeBehaviours[Int, Long] {
  override val day = "Day ten"
  override val file: String = "Day10Input"
  override val dayChallenge: DayChallenge[Int,Long] = DayTen

  private val testInput = dayChallenge.testData

  day should behave like partOneWorkingCorrectly(testInput, dayChallenge.expectedPartOne.get)

  it should behave like partOneWorksOnRealData(2812)

  it should behave like partTwoWorksOnTestData(testInput, dayChallenge.expectedPartTwo.getOrElse(0))

  it should behave like partTwoWorksOnRealData("386869246296064".toLong)
}
