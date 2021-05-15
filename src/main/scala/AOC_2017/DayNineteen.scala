package AOC_2017

import shared.{DayChallenge, TestData}

import scala.util.Try

object DayNineteen extends DayChallenge[String, Int] {
  override def partOne(l: List[String]): String = Grid(l.map(_.toCharArray.toList)).traverse._1.mkString

  case class Grid(rows: List[List[Char]]) {
    def get(coord: (Int, Int)): Option[Char] = Try(rows(coord._2)(coord._1)).toOption

    def traverse: (List[Char], Int) = {
      def go(currentCoord: (Int, Int), numberOfSteps: Int,  currentDirection: Direction, lettersEncountered: List[Char]): (List[Char], Int) = {
        get(currentCoord) match {
          case Some(char) =>
            val newLettersEncountered = if ("[A-Z]".r.matches(char.toString)) lettersEncountered :+ char else lettersEncountered
            currentDirection match {
              case Left | Right | Up | Down =>
                val nextCoord = neighbouringCoord(currentDirection, currentCoord)
                val newDirection = get(nextCoord) match {
                  case Some('+') =>
                    currentDirection match {
                      case Left | Right => UpOrDown
                      case _ => LeftOrRight
                    }
                  case _ => currentDirection
                }
                go(nextCoord, numberOfSteps + 1, newDirection, newLettersEncountered)
              case unknownDirection @ _  =>
                val newDirection = chooseDirection(currentCoord, unknownDirection)
                val nextCoord = neighbouringCoord(newDirection, currentCoord)
                go(nextCoord, numberOfSteps + 1, newDirection, newLettersEncountered)
            }
          case _ => (lettersEncountered, numberOfSteps)
        }
      }
      go((rows.head.indexOf('|'), 0), 0, Down, List.empty)
    }

    def chooseDirection(currentCoord: (Int, Int), currentDirection: Direction): Direction =
      currentDirection match {
        case LeftOrRight =>
          get(neighbouringCoord(Right, currentCoord)) match {
            case Some(neighbouringRight) if neighbouringRight != ' ' => Right
            case _ => Left
          }
        case UpOrDown =>
          get(neighbouringCoord(Up, currentCoord)) match {
            case Some(neighbouringUp) if neighbouringUp != ' ' => Up
            case _ => Down
          }
        case other @ _ => other
      }

    def neighbouringCoord(direction: Direction, currentCoord: (Int, Int)): (Int, Int) =
      direction match {
        case Up => (currentCoord._1, currentCoord._2 - 1)
        case Down => (currentCoord._1, currentCoord._2 + 1)
        case Left => (currentCoord._1 - 1, currentCoord._2)
        case Right => (currentCoord._1 + 1, currentCoord._2)
      }
  }

  override def partTwo(l: List[String]): Int = Grid(l.map(_.toCharArray.toList)).traverse._2

  trait UncertainDirection

  trait Direction
  case object Up extends Direction
  case object Down extends Direction
  case object Right extends Direction
  case object Left extends Direction
  case object UpOrDown extends Direction with UncertainDirection
  case object LeftOrRight extends Direction with UncertainDirection

}

object DayNineteenData extends TestData[String, Int] {
  override val expectedPartOne: Option[String] = Some("ABCDEF")
  override val expectedPartTwo: Option[Int] = Some(38)
  override val testData: List[String] =
    List("     |         ",
      "     |  +--+   ",
      "     A  |  C   ",
      " F---|----E|--+",
      "     |  |  |  D",
      "     +B-+  +--+")
}
