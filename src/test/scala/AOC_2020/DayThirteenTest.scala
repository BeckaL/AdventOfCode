package AOC_2020

import org.scalatest.flatspec.AnyFlatSpec
import shared.{DayChallenge, DayChallengeBehaviours}

class DayThirteenTest extends AnyFlatSpec with DayChallengeBehaviours[Int, Long] {
  override val day = "Day thirteen"
  override val file: String = "Day13Input"
  override val dayChallenge: DayChallenge[Int,Long] = DayThirteen

  private val testInput = dayChallenge.testData

  day should behave like partOneWorkingCorrectly(testInput, dayChallenge.expectedPartOne.get)

  it should behave like partOneWorksOnRealData(2406)

  it should behave like partTwoWorksOnTestData(testInput, dayChallenge.expectedPartTwo.getOrElse(0))

  it should behave like partTwoWorksOnRealData("225850756401039".toLong)

}
