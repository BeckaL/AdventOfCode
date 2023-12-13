package AOC_2020

import org.scalatest.flatspec.AnyFlatSpec
import shared.{DayChallenge, DayChallengeBehaviours}

class DayNineTest extends AnyFlatSpec with DayChallengeBehaviours[BigInt, BigInt] {
  override val day = "Day nine"
  override val file: String = "Day9Input"
  override val dayChallenge: DayChallenge[BigInt, BigInt] = DayNine

  private val testInput = dayChallenge.testData

  day should behave like partOneWorkingCorrectly(testInput, dayChallenge.expectedPartOne.get)

  it should behave like partOneWorksOnRealData(530627549)

  it should behave like partTwoWorksOnTestData(testInput, dayChallenge.expectedPartTwo.getOrElse(0))

  it should behave like partTwoWorksOnRealData(77730285)
}
