package AOC_2023

import shared.{DayChallenge, TestData}

object DayOne extends DayChallenge[Int, Int]{
  override def partOne(l: List[String]): Int = 
    l.map(getCalibrationValue).sum

  private def getCalibrationValue(line: String): Int = {
    val numbers = line.filter(c => c.isDigit)
    s"${numbers.head}${numbers.last}".toInt
  }

  override def partTwo(l: List[String]): Int =
    l.map(getCalibrationValuePart2).sum

  private def getCalibrationValuePart2(line: String): Int = {
    s"${findDigit(line)}${findDigit(line.reverse, true)}".toInt
  }

  private def findDigit(s: String, reverse: Boolean = false): String = {
    val firstDigit = s.find(c => c.isDigit)
    val indexOfFirstDigit = firstDigit match {
      case Some(d) => s.indexOf(d)
      case None => -1
    }
    val digitsToSearch = if (reverse) reverseDigits else digits
    val possibleDigitsAndIndices = digitsToSearch.map(i => (i, s.indexOf(i))).filterNot(t => t._2 == -1)
    if (possibleDigitsAndIndices.nonEmpty) {
      val (numberAsString, indexOfFirstStringDigit) = possibleDigitsAndIndices.minBy(_._2)
      if (indexOfFirstDigit < indexOfFirstStringDigit && indexOfFirstDigit != -1) {
        s(indexOfFirstDigit).toString
      } else {
        (digitsToSearch.indexOf(numberAsString) + 1).toString
      }
    } else {
      s(indexOfFirstDigit).toString
    }
  }

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