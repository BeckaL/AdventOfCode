package AOC_2015

import shared.{DayChallenge, YearController}

object Controller2015 extends YearController {
  def runPart(day: String, part: String, input: List[String]) = {
    println(s"running part $part of day $day 2015")
    val d = day match {
      case "1" => DayOne
      case "2" => DayTwo
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
      case _ => throw new RuntimeException("Didn't understand that day")
    }
  }

  private def printAnswers[A, B](day: DayChallenge[A, B], input: List[String]): Unit = {
    val testAns1 = day.partOne(day.testData)
    val realAns1 = day.partOne(input)
    val testAns2 = day.partTwo(day.testData2.getOrElse(day.testData))
    val realAns2 = day.partTwo(input)

    if (day.expectedPartTwo.contains(testAns2)) {
      println(s"GO part two!!! $realAns2")
    }
    if (day.expectedPartOne.contains(testAns1)) {
      println(s"GO part one!!! $realAns1")
    }

    println(s"Part 1 test answer is $testAns1")
    println(s"Part 1 real answer is $realAns1")

    println(s"Part 2 test answer is $testAns2")
    println(s"Part 2 real answer is $realAns2")
  }

}
