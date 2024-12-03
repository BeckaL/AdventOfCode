package AOC_2024

import shared.{DayChallenge, Helpers, TestData}

import scala.annotation.tailrec

object DayThree extends DayChallenge[Long, Long] with Helpers{
  override def partOne(l: List[String]): Long =
    l.map(getSumOfMultiplyInstructions).sum

  override def partTwo(l: List[String]): Long =
    l.foldLeft((0.toLong, true)){case ((soFar, enabled), string) =>
      getSumOfMultiplyInstructions(string, enabled, soFar)
    }._1

  private val multiplyRegex = """mul\((\d+),(\d+)\)""".r

  private def getSumOfMultiplyInstructions(s: String): Long =
    multiplyRegex.findAllIn(s).toList.map(extractInts(_).product).sum

  @tailrec
  private def getSumOfMultiplyInstructions(s: String, enabled: Boolean, soFar: Long): (Long, Boolean) =
    val presentInstructions = getPresentInstructions(s)
    if (presentInstructions.isEmpty)
      (soFar, enabled)
    else
      val (newS, newEnabled, newSoFar) = presentInstructions.minBy(_._1) match
        case (i, "do") => (s.drop(i + 4), true, soFar)
        case (i, "dont") => (s.drop(i + 6), false, soFar)
        case (i, instructionString) =>
         val soFarAddition = if (enabled) extractInts(instructionString).product else 0
         (s.drop(i + instructionString.length), enabled, soFar + soFarAddition)
      getSumOfMultiplyInstructions(newS, newEnabled, newSoFar)

  private def getPresentInstructions(s: String) =
    List(
      (s.indexOf("do()"), "do"),
      (s.indexOf("don't()"), "dont"),
      multiplyRegex.findFirstMatchIn(s).map(m => (m.start, m.toString)).getOrElse((-1, ""))
    ).filter(_._1 >= 0)
}

object DayThreeData extends TestData[Long, Long] {
  override val testData: List[String] = List(
    "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
  )
  override val testData2: Option[List[String]] = Some(List("xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"))
  override val expectedPartOne: Option[Long] = Some(161)
  override val expectedPartTwo: Option[Long] = Some(48)
}