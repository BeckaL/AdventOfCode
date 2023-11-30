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
    }

  override def printAnswers(day: String, input: List[String]): Unit = {
    val (d, data) = getDayChallengeAndData(day)
    printAnswers(d, input, data)
  }
}