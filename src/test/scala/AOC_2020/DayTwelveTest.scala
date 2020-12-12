package AOC_2020

import org.scalatest.FlatSpec
import shared.{DayChallenge, DayChallengeBehaviours}

class DayTwelveTest extends FlatSpec with DayChallengeBehaviours[Int, Int] {
  override val day = "Day twelve"
  override val file: String = "Day12Input"
  override val dayChallenge: DayChallenge[Int, Int] = DayTwelve

  private val testInput = dayChallenge.testData

  day should behave like partOneWorkingCorrectly(testInput, dayChallenge.expectedPartOne.get)

  it should behave like partOneWorksOnRealData(439)

  it should behave like partTwoWorksOnTestData(testInput, dayChallenge.expectedPartTwo.getOrElse(0))

  it should behave like partTwoWorksOnRealData(12385)
}
