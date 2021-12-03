package AOC_2021

import shared.{DayChallenge, Helpers, TestData}

object DayTwo extends DayChallenge[Int, Int] with Helpers {
  override def partOne(l: List[String]): Int =
    getFinalPosition(getInstructions(l))

  def getInstructions(l: List[String]): List[(String, Int)] =
    l.map(str => getTwoFromSplit(str, " "))
      .map{ case (instruction, depthString) =>(instruction, depthString.toInt)}

  def getFinalPosition(instructions: List[(String, Int)]): Int = {
    val (finalDepth, finalDistance) = instructions.foldLeft((0, 0)) { case ((currentDepth, currentDistance), instruction) =>
      instruction match {
        case ("forward", dist) => (currentDepth, currentDistance + dist)
        case ("up", x) => (currentDepth - x, currentDistance)
        case (_, x) => (currentDepth + x, currentDistance)
      }
    }
    finalDepth * finalDistance
  }

  override def partTwo(l: List[String]): Int = getFinalPositionWithAim(getInstructions(l))

  def getFinalPositionWithAim(instructions: List[(String, Int)]): Int = {
    val (_, finalDepth, finalDistance) =  instructions.foldLeft((0, 0, 0)){ case ((currentAim, currentDepth, currentDistance), instruction) =>
      instruction match {
          case ("forward", dist) => (currentAim, currentDepth + (dist * currentAim), currentDistance + dist)
          case ("up", x) => (currentAim - x, currentDepth, currentDistance)
          case (_, x) => (currentAim + x, currentDepth, currentDistance)
        }
    }
    finalDepth * finalDistance
  }
}

object DayTwoData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "forward 5",
    "down 5",
    "forward 8",
    "up 3",
    "down 8",
    "forward 2"
  )
  override val expectedPartOne: Option[Int] = Some(150)
  override val expectedPartTwo: Option[Int] = Some(900)
}
