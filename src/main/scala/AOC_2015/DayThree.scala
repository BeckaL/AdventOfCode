package AOC_2015

import shared.{Coord, DayChallenge, GridHelpers}

object DayThree extends DayChallenge[Int, Int] with GridHelpers {

  override def partOne(l: List[String]): Int =
    l.head.foldLeft(startingList)((coords, char) => getNewCoords(coords, char)).toSet.size

  override def partTwo(l: List[String]): Int = {
    val combined = l.head
      .grouped(2)
      .toList
      .foldLeft((startingList, startingList)) {
        case ((santaVisited, robocopVisited), chars) =>
          (getNewCoords(santaVisited, chars(0)), getNewCoords(robocopVisited, chars(1)))
      }
    (combined._1 ++ combined._2).toSet.size
  }

  implicit class CoordOps(c: Coord) {
    def update(char: Char) = {
      char match {
        case '^' => c.copy(y = c.y + 1)
        case 'v' => c.copy(y = c.y - 1)
        case '>' => c.copy(x = c.x + 1)
        case '<' => c.copy(x = c.x - 1)
      }
    }
  }

  private def getNewCoords(coords: List[Coord], char: Char) = coords.head.update(char) +: coords

  private val startingList = List(Coord(0, 0))

  override val testData: List[String] = List("^>v<")
  override val expectedPartOne: Option[Int] = Some(4)
  override val expectedPartTwo: Option[Int] = Some(3)
}
