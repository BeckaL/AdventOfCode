package AOC_2023

import shared.{DayChallenge, TestData}

object DayOne extends DayChallenge[Int, Int]{
  override def partOne(l: List[String]): Int = 
    l.map(getCalibrationValue).sum

  private def getCalibrationValue(line: String): Int =
    val numbers = line.filter(c => c.isDigit)
    s"${numbers.head}${numbers.last}".toInt

  override def partTwo(l: List[String]): Int =
    l.map(getCalibrationValuePart2).sum

  private def getCalibrationValuePart2(line: String): Int =
    s"${findFirstDigit(line, digits)}${findFirstDigit(line.reverse, reverseDigits)}".toInt

  private def findFirstDigit(s: String, digitsAsStrings: List[String]): String =
    if (s.head.isDigit)
      s.head.toString
    else
      digitsAsStrings.find(digitString => s.startsWith(digitString)) match
        case Some(digitString) => (digitsAsStrings.indexOf(digitString) + 1).toString
        case None => findFirstDigit(s.tail, digitsAsStrings)

  private val digits = List("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  private val reverseDigits = digits.map(_.reverse)
}

object DayOneData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "1abc2",
      "pqr3stu8vwx",
      "a1b2c3d4e5f",
      "treb7uchet"
  )
  override val testData2: Option[List[String]] = Some(List(
  "two1nine",
  "eightwothree",
  "abcone2threexyz",
  "xtwone3four",
  "4nineeightseven2",
  "zoneight234",
  "7pqrstsixteen",
  ))
  override val expectedPartOne: Option[Int] = Some(142)
  override val expectedPartTwo: Option[Int] = Some(281)
}