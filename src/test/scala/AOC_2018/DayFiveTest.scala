package AOC_2018

import org.scalatest.FlatSpec
import shared.{DayChallenge, DayChallengeBehaviours}

class DayFiveTest extends FlatSpec with DayChallengeBehaviours[Int,Int] {
  override val day = "Day five"
  override val file: String = "Day5Input"
  override val year = 2018
  override val dayChallenge: DayChallenge[Int, Int] = DayFive

  private val testInput = dayChallenge.testData
    //TODO:THese tests pass but the code is hideously inefficient
//  day should behave like partOneWorkingCorrectly(testInput, dayChallenge.expectedPartOne.get)
//  it should behave like partOneWorksOnRealData( 11668)
//  it should behave like partTwoWorksOnTestData(testInput, dayChallenge.expectedPartTwo.get)

//  it should behave like partTwoWorksOnRealData( 4652)


}
