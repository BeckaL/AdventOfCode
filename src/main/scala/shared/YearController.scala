package shared

trait YearController {
  //TODO: just mandate a 'get day and data' method
  def runPart(day: String, part: String, input: List[String]): Unit
  def printAnswers(day: String, input: List[String]): Unit

  //Allows separation of test data from main code, and also allows expected test answer to be none, in which case the code is run against main data
  def runPartFromTestData[A, B](d: DayChallenge[A, B], part: String, input: List[String], testData: TestData[A, B]): Unit = {
    part match {
      case "1" =>
        val ans = d.partOne(testData.testData)
        println(ans)
        testData.expectedPartOne match {
          case Some(expectedPart1) =>
            if (expectedPart1 == ans) {
              println("is correct")
              println(s"GO part one!!! ${d.partOne(input)}")
            } else println("not correct")
          case None =>
            println(s"GO part one!!! ${d.partOne(input)}")
        }
      case "2" =>
        val ans = d.partTwo(testData.testData2.getOrElse(testData.testData))
        println(ans)
        testData.expectedPartTwo match {
          case Some(expectedPart2) =>
            if (expectedPart2 == ans) {
              println("is correct")
              println(s"GO part two!!! ${d.partTwo(input)}")
            } else println("not correct")
          case None =>
            println(s"GO part two!!! ${d.partTwo(input)}")
        }
    }
  }

  //TODO: remove this and the other method, all should be supplying test data object
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

  //Allows separation of test data from main code, and also allows expected test answer to be none, in which case the code is run against main data
  def printAnswers[A, B](day: DayChallenge[A, B], realInput: List[String], testData: TestData[A, B]): Unit = {
    val (testAns1, realAns1) = testData.expectedPartOne match {
      case Some(expectedPartOne) =>
        val testAns = day.partOne(testData.testData)
        val realAns = day.partOne(realInput)
        if (testAns == expectedPartOne) {
          val realAns = day.partOne(realInput)
          println(s"GO part one!!! $realAns")
        }
        (Some(testAns), realAns)
      case None =>
        val realAns = day.partOne(realInput)
        println(s"GO part one!!! $realAns")
        (None, realAns)
    }
    val (testAns2, realAns2) = testData.expectedPartTwo match {
      case Some(expectedPartTwo) =>
        val testAns = day.partTwo(testData.testData2.getOrElse(testData.testData))
        val realAns = day.partTwo(realInput)
        if (testAns == expectedPartTwo) {
          val realAns = day.partTwo(realInput)
          println(s"GO part two!!! $realAns")
        }
        (Some(testAns), realAns)
      case None =>
        val realAns = day.partTwo(realInput)
        println(s"GO part two!!! $realAns")
        (None, realAns)
    }

    println(testAns1.map(ans1 => s"Part 1 test answer is $ans1").getOrElse("Didn't have an expected ans to part 1 to run on test data"))
    println(s"Part 1 real answer is $realAns1")

    println(testAns2.map(ans2 => s"Part 2 test answer is $ans2").getOrElse("Didn't have an expected ans to part 2 to run on test data"))
    println(s"Part 2 real answer is $realAns2")
    }
}
