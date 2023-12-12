package AOC_2023

import shared.{DayChallenge, TestData, YearController}

object Controller2023 extends YearController {
  override def runPart(day: String, part: String, input: List[String]) = {
    println(s"running part $part of day $day 2022")
    val (d, data) = getDayChallengeAndData(day)
    runPartFromTestData(d, part, input, data)
  }

  def getDayChallengeAndData(day: String): (DayChallenge[_, _], TestData[_, _]) =
    day match {
      case "1" => (DayOne, DayOneData)
      case "2" => (DayTwo, DayTwoData)
      case "3" => (DayThree, DayThreeData)
      case "4" => (DayFour, DayFourData)
      case "5" => (DayFive, DayFiveData)
      case "6" => (DaySix, DaySixData)
      case "7" => (DaySeven, DaySevenData)
      case "8" => (DayEight, DayEightData)
      case "9" => (DayNine, DayNineData)
      case "10" => (DayTen, DayTenData)
      case "11" => (DayEleven, DayElevenData)
//      case "12" => (DayTwelve, DayTwelveData)
    }

  override def printAnswers(day: String, input: List[String]): Unit = {
    val (d, data) = getDayChallengeAndData(day)
    printAnswers(d, input, data)
  }
}