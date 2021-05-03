package shared

trait YearController {
  def runPart(day: String, part: String, input: List[String]): Unit
  def printAnswers(day: String, input: List[String]): Unit

  def printAnswers[A, B](day: DayChallenge[A, B], input: List[String]): Unit = {
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
