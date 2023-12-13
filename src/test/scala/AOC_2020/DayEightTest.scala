package AOC_2020

import org.scalatest.flatspec.AnyFlatSpec
import shared.{DayChallenge, DayChallengeBehaviours}

class DayEightTest extends AnyFlatSpec with DayChallengeBehaviours[Int, Int] {
  override val day = "Day eight"
  override val file: String = "Day8Input"
  override val dayChallenge: DayChallenge[Int, Int] = DayEight

  private val testInput = dayChallenge.testData

  day should behave like partOneWorkingCorrectly(testInput, dayChallenge.expectedPartOne.get)

  it should behave like partOneWorksOnRealData(1614)

  it should behave like partTwoWorksOnTestData(testInput, dayChallenge.expectedPartTwo.getOrElse(100000))

  it should behave like partTwoWorksOnRealData(1260)
}
