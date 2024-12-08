package AOC_2024

import shared.{DayChallenge, TestData, YearController}

object Controller2024 extends YearController {
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
//      case "8" => (DayEight, DayEightData)
//      case "9" => (DayNine, DayNineData)
//      case "10" => (DayTen, DayTenData)
//      case "11" => (DayEleven, DayElevenData)
//      case "12" => (DayTwelve, DayTwelveData)
//      case "13" => (DayThirteen, DayThirteenData)
//      case "14" => (DayFourteen, DayFourteenData)
//      case "15" => (DayFifteen, DayFifteenData)
//      case "16" => (DaySixteen, DaySixteenData)
//      case "17" => (DaySeventeen, DaySeventeenData)
//      case "18" => (DayEighteen, DayEighteenData)
//      case "19" => (DayNineteen, DayNineteenData)
//      case "20" => (DayTwenty, DayTwentyData)
//      case "21" => (DayTwentyOne, DayTwentyOneData)
//      case "22" => (DayTwentyTwo, DayTwentyTwoData)
//      case "23" => (DayTwentyThree, DayTwentyThreeData)
//      case "24" => (DayTwentyFour, DayTwentyFourData)
//      case "25" => (DayTwentyFive, DayTwentyFiveData)
    }

  override def printAnswers(day: String, input: List[String]): Unit = {
    val (d, data) = getDayChallengeAndData(day)
    printAnswers(d, input, data)
  }
}
