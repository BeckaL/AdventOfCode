package AOC_2020

import org.scalatest.FlatSpec
import shared.{DayChallenge, DayChallengeBehaviours}

class DayFiveTest extends FlatSpec with DayChallengeBehaviours[Int, Int] {
  override val day = "Day five"
  override val file: String = "Day5Input"
  override val dayChallenge: DayChallenge[Int, Int] = DayFive

  private val testInput = List[String]("FBFBBFFRLR")

  day should behave like partOneWorkingCorrectly(testInput, 357)

  it should behave like partOneWorksOnRealData(864)

  it should behave like partTwoWorksOnRealData(739)
}
