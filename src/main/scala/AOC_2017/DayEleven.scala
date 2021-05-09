package AOC_2017

import shared.DayChallenge

object DayEleven extends DayChallenge[Int, Int] {

  override def partOne(l: List[String]): Int = {
    val end = getEndCoord(l.head.split(",").toList)
    findRouteToOriginFrom(end._1, end._2, 0)
  }

  private def getEndCoord(instructions: List[String]): (Int, Int) =
    instructions.foldLeft((0, 0)){case ((x, y), direction) => move(direction, x, y)}

  private def findRouteToOriginFrom(x: Int, y: Int, numberOfSteps: Int): Int =
    if ((x, y) == (0, 0)) {
      numberOfSteps
    } else {
      val yDirection = if (y > 0) "s" else "n"
      val xDirection = x match {
        case 0 => ""
        case x => if (x > 0) "w" else "e"
      }
      val (newX, newY) = move(yDirection + xDirection, x, y)
      findRouteToOriginFrom(newX, newY, numberOfSteps + 1)
    }

  def move(direction: String, x: Int, y: Int): (Int, Int) =
    direction match {
      case "s" => (x, y - 2)
      case "n" => (x, y + 2)
      case "sw" => (x - 2, y - 1)
      case "nw" => (x - 2, y + 1 )
      case "se" => (x + 2, y - 1)
      case "ne" => (x + 2, y + 1)
    }

  //TODO: Brute force atm, work out nicer way
  override def partTwo(l: List[String]): Int = {
    l.head.split(",").toList.foldLeft(((0, 0), 0)) { case (((x, y), numberOfSteps), direction) =>
      val newCoord = move(direction, x, y)
      val newNumberOfSteps = findRouteToOriginFrom(newCoord._1, newCoord._2, 0)
      val biggestNumberOfSteps = if (newNumberOfSteps > numberOfSteps) newNumberOfSteps else numberOfSteps
      (newCoord, biggestNumberOfSteps)
    }
  }._2

  override val expectedPartOne: Option[Int] = Some(3)
  override val testData: List[String] = List("se,sw,se,sw,sw")
  override val expectedPartTwo: Option[Int] = Some(3)
  }
