package AOC_2024

import shared.{Coord, DayChallenge, Helpers, TestData}

object DayFourteen extends DayChallenge[Int, Int] with Helpers {
  override def partOne(l: List[String]): Int = 
    val xMax = if(l == DayFourteenData.testData) 11 else 101
    val yMax = if (l == DayFourteenData.testData) 7 else 103
    val robots = l.map(parsePositionAndVelocity)
    val finalRobots = (1 to 100).foldLeft(robots)((currentRobots, _) =>
      currentRobots.map(r => move(xMax, yMax, r._1, r._2)))
    countRobotsInQuadrants(xMax, yMax, finalRobots.map(_._1))

  private def move(xMax: Int, yMax: Int, c: Coord, v: (Int, Int)) =
    (Coord((c.x + v._1 + xMax) % xMax, (c.y + v._2 + yMax) % yMax), v)

  private def print(xMax: Int, yMax: Int, positions: List[Coord]) =
    val g = (0 until yMax).map(y =>
      (0 until xMax).map(x =>
        val count = positions.count(_ == Coord(x, y))
        if (count == 0) '.' else count.toString.head
      ).mkString).mkString("\n")
    println(g)

  private def countRobotsInQuadrants(xMax: Int, yMax: Int, positions: List[Coord]) =
    val (middleX, middleY) = (xMax / 2 , yMax / 2)
    val q1 = positions.count(c => c.x < middleX && c.y < middleY)
    val q2 = positions.count(c => c.x > middleX && c.y < middleY)
    val q3 = positions.count(c => c.x < middleX && c.y > middleY)
    val q4 = positions.count(c => c.x > middleX && c.y > middleY)
    q1 * q2 * q3 * q4

  private def parsePositionAndVelocity(s: String) =
    val ints = extractIntsWithOptionalSigns(s)
    (Coord(ints.head, ints(1)), (ints(2), ints(3)))


  override def partTwo(l: List[String]): Int = 
    //This doesn't actually return the answer. Instead, it allows visual inspection to find the answer
    val xMax = if (l == DayFourteenData.testData) 11 else 101
    val yMax = if (l == DayFourteenData.testData) 7 else 103
    val robots = l.map(parsePositionAndVelocity)
    if (l == DayFourteenData.testData) {
      return 0
    }
    //After watching the patterns for a while, noticed that every once in a while there'd be
    // something that looked less random.
    //Worked out the frequency of these, and print these out
    val finalRobots = (1 to 8000).foldLeft(robots)((currentRobots, i) =>
      val newRobots = currentRobots.map(r => move(xMax, yMax, r._1, r._2))
      //After finding the answer, narrows it down so not as much has to be printed to repeat
      if (i > 7000 && (i % xMax == 38)) {
        println(s"iteration ${i} ------ ")
        print(xMax, yMax, newRobots.map(_._1))
        Thread.sleep(150)
      }
      newRobots
    )
    0
}

object DayFourteenData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "p=0,4 v=3,-3",
    "p=6,3 v=-1,-3",
    "p=10,3 v=-1,2",
    "p=2,0 v=2,-1",
    "p=0,0 v=1,3",
    "p=3,0 v=-2,-2",
    "p=7,6 v=-1,-3",
    "p=3,0 v=-1,-2",
    "p=9,3 v=2,3",
    "p=7,3 v=-1,2",
    "p=2,4 v=2,-3",
    "p=9,5 v=-3,-3"
  )
  override val expectedPartOne: Option[Int] = Some(12)
  override val expectedPartTwo: Option[Int] = None
  override val testData2: Option[List[String]] = None
}