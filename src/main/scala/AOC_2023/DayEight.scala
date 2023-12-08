package AOC_2023

import shared.{DayChallenge, Helpers, TestData}

import scala.annotation.tailrec

object DayEight extends DayChallenge[Long, Long] with Helpers {
  override def partOne(l: List[String]): Long =
    traverseUntilEndCondition(l, (s: String) => s == "ZZZ", "AAA")

  override def partTwo(l: List[String]): Long = {
    val starts = parseMap(l).keys.filter(_.endsWith("A"))
    val shortestPaths = starts.map(start => traverseUntilEndCondition(l, (s: String) => s.endsWith("Z"), start))
    println(shortestPaths)
    //Then put this into Wolfram alpha to find LCM
    shortestPaths.product
  }

  private def traverseUntilEndCondition(input: List[String], endCondition: String => Boolean, start: String): Long =
    val instructions = input.head.split("").toList
    val m = parseMap(input)

    @tailrec
    def go(current: String, stepsSoFar: Long, instructionIndex: Int): Long =
      if (endCondition(current))
        stepsSoFar
      else
        val (left, right) = m(current)
        val next = if (instructions(instructionIndex) == "L") left else right
        val nextInstructionIndex = if (instructionIndex == instructions.size - 1 ) 0 else instructionIndex + 1
        go(next, stepsSoFar + 1, nextInstructionIndex)
    go(start, 0L, 0)

  private def parseMap(l: List[String]) =
    l.drop(2).map(line =>
      val (start, destinations) = getTwoFromSplit(line, " = ")
      val (left, right) = getTwoFromSplit(destinations.tail.dropRight(1), ", ")
      start -> (left, right)
    ).toMap
}

object DayEightData extends TestData[Long, Long] {
  override val testData: List[String] = List(
    "LLR",
    "",
    "AAA = (BBB, BBB)",
    "BBB = (AAA, ZZZ)",
    "ZZZ = (ZZZ, ZZZ)"
  )
  override val expectedPartOne: Option[Long] = Some(6L)
  override val expectedPartTwo: Option[Long] = Some(6)
}