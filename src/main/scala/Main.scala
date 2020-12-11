import AOC_2020._
import shared.DayChallenge

import scala.io.Source

object AOC {
  def main(args: Array[String]): Unit = {
    val bufferedSource = Source.fromFile(s"/Users/rlq3651/Projects/AOC/src/test/resources/2020/Day${args.head}Input.txt")
    val input = bufferedSource.getLines().toList
    bufferedSource.close()
    if (args.size > 1) {
      runPart(args.head, args(1))
    } else {
      printAnswers(args.head, input)
    }
  }

  private def runPart(day: String, part: String) = {
    println(s"running part $part of day $day")
    val d = day match {
      case "1" => DayOne
      case "2" => DayTwo
      case "3" => DayThree
      case "4" => DayFour
      case "5" => DayFive
      case "6" => DaySix
      case "7" => DaySeven
      case "8" => DayEight
      case "9" => DayNine
      case "10" => DayTen
      case "11" => DayEleven
      case _ => throw new RuntimeException("Didn't understand that day")
    }
    part match {
      case "1" => println(d.partOne(d.testData))
      case "2" => println(d.partTwo(d.testData))
    }
  }


  private def printAnswers(day: String, input: List[String]): Unit = {
    day match {
      case "1" => printAnswers(DayOne, input)
      case "2" => printAnswers(DayTwo, input)
      case "3" => printAnswers(DayThree, input)
      case "4" => printAnswers(DayFour, input)
      case "5" => printAnswers(DayFive, input)
      case "6" => printAnswers(DaySix, input)
      case "7" => printAnswers(DaySeven, input)
      case "8" => printAnswers(DayEight, input)
      case "9" => printAnswers(DayNine, input)
      case "10" => printAnswers(DayTen, input)
      case "11" => printAnswers(DayEleven, input)
      case _ => throw new RuntimeException("Didn't understand that day")
    }
  }

  private def printAnswers[A, B](day: DayChallenge[A, B], input: List[String]): Unit = {
    val testAns1 = day.partOne(day.testData)
    val realAns1 = day.partOne(input)
    val testAns2 = day.partTwo(day.testData)
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
