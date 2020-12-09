package AOC_2020

import org.scalatest.FlatSpec
import shared.{DayChallenge, DayChallengeBehaviours}

class DaySixTest extends FlatSpec with DayChallengeBehaviours[Int, Int] {
  override val day = "Day six"
  override val file: String = "Day6Input"
  override val dayChallenge: DayChallenge[Int, Int] = DaySix

  private val testInput = List[String]("abc",
    "",
    "a",
    "b",
    "c",
    "",
    "ab",
    "ac",
    "",
    "a",
    "a",
    "a",
    "a",
    "",
    "b")

  day should behave like partOneWorkingCorrectly(testInput, 11)

  it should behave like partOneWorksOnRealData(6249)

  it should behave like partTwoWorksOnTestData(testInput, 6)

  it should behave like partTwoWorksOnRealData(3103)
}
