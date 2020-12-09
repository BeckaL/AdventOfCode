package AOC_2018

import org.scalatest.FlatSpec
import shared.{DayChallenge, DayChallengeBehaviours}

class DayFour2018Test extends FlatSpec with DayChallengeBehaviours[Int, Int] {
  override val day = "Day four"
  override val file: String = "Day4Input"
  override val dayChallenge: DayChallenge[Int, Int] = DayFour
  override val year = 2018

  private val testInput = dayChallenge.testData

  day should behave like partOneWorkingCorrectly(testInput, 240)

//  it should behave like partOneWorksOnRealData(0)
}
