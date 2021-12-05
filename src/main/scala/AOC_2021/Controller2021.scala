package AOC_2021
import shared.{DayChallenge, TestData, YearController}


object Controller2021 extends YearController {
    override def runPart(day: String, part: String, input: List[String]) = {
      println(s"running part $part of day $day 2021")
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
        case _ => throw new RuntimeException("Didn't understand that day")
      }

    override def printAnswers(day: String, input: List[String]): Unit = {
      val (d, data) = getDayChallengeAndData(day)
      printAnswers(d, input, data)
    }
  }
