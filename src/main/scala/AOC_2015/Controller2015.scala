package AOC_2015

import shared.{DayChallenge, YearController}

object Controller2015 extends YearController {
  def runPart(day: String, part: String, input: List[String]) = {
    println(s"running part $part of day $day 2015")
    val d = day match {
      case "1" => DayOne
      case "2" => DayTwo
      case "3" => DayThree
      case _ => throw new RuntimeException("Didn't understand that day")
    }
    part match {
      case "1" =>
        val ans = d.partOne(d.testData)
        if (d.expectedPartOne contains ans) {
          println(ans)
          println("is correct")
          println(d.expectedPartOne)
          println(s"GO part one!!! ${d.partOne(input)}")
        } else {
          println(ans)
        }
      case "2" =>
        val ans = d.partTwo(d.testData2.getOrElse(d.testData))
        if (d.expectedPartTwo contains ans) {
          println(ans)
          println("is correct")
          println(s"GO part two!!! ${d.partTwo(input)}")
        } else {
          println(ans)
        }
    }
  }


  def printAnswers(day: String, input: List[String]): Unit = {
    day match {
      case "1" => printAnswers(DayOne, input)
      case "2" => printAnswers(DayTwo, input)
      case "3" => printAnswers(DayThree, input)
      case _ => throw new RuntimeException("Didn't understand that day")
    }
  }
}
