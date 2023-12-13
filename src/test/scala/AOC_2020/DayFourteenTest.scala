package AOC_2020

import org.scalatest.flatspec.AnyFlatSpec
import shared.{DayChallenge, DayChallengeBehaviours}

class DayFourteenTest extends AnyFlatSpec with DayChallengeBehaviours[Long, Long] {
  override val day = "Day fourteen"
  override val file: String = "Day14Input"
  override val dayChallenge: DayChallenge[Long,Long] = DayFourteen

  private val testInput = dayChallenge.testData

  day should behave like partOneWorkingCorrectly(testInput, dayChallenge.expectedPartOne.get)

  it should behave like partOneWorksOnRealData(0)

  it should behave like partTwoWorksOnTestData(testInput, dayChallenge.expectedPartTwo.getOrElse(0))

  it should behave like partTwoWorksOnRealData(0)
}
