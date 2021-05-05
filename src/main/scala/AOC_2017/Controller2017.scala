package AOC_2017

import shared.YearController

object Controller2017 extends YearController {
  override def runPart(day: String, part: String, input: List[String]) = {
    println(s"running part $part of day $day 2017")
    val d = day match {
      case "1" => DayOne
      case "2" => DayTwo
      case "3" => DayThree
      case "4" => DayFour
      case "5" => DayFive
      case "6" => DaySix
      case "7" => DaySeven
      case "8" => DayEight
      case "13" => DayThirteen
      case "16" => DaySixteen
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

  override def printAnswers(day: String, input: List[String]): Unit = day match {
    case "1" => printAnswers(DayOne, input)
    case "2" => printAnswers(DayTwo, input)
    case "3" => printAnswers(DayThree, input)
    case "4" => printAnswers(DayFour, input)
    case "5" => printAnswers(DayFive, input)
    case "6" => printAnswers(DaySix, input)
    case "7" => printAnswers(DaySeven, input)
    case "8" => printAnswers(DayEight, input)
    case "13" => printAnswers(DayThirteen, input)
    case "16" => printAnswers(DaySixteen, input)
    case _ => throw new RuntimeException("Didn't understand that day")
  }
}