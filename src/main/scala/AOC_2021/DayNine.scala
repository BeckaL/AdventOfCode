package AOC_2021

import shared.{Coord, DayChallenge, GridHelpers, TestData}

object DayNine extends DayChallenge[Int, Int] {
  override def partOne(l: List[String]): Int = {
    val grid = l.map(str => str.split("").map(_.toInt).toList)
    val coords = for {
      x <- grid.head.indices
      y <- grid.indices
    } yield Coord(x, y)

    coords.filter(c => isSmallerThanAllNeighbours(c, grid)).map(c => grid(c.y)(c.x) + 1).sum
  }

  def isSmallerThanAllNeighbours(c: Coord, g: List[List[Int]]): Boolean =
    getNeighbours(c, g).forall(neighbour => g(c.y)(c.x) < neighbour)

  private def getNeighbours(c: Coord, g: List[List[Int]]): List[Int] =
    List((0, 1), (1, 0), (-1, 0), (0, -1)).map { case (xChange, yChange) =>
      Coord(c.x + xChange, c.y + yChange)
    }.filter(isInGrid(_, g)).map { coordInGrid =>
      g(coordInGrid.y)(coordInGrid.x)
    }

  def isInGrid(c: Coord, g: List[List[Int]]) = g.head.indices.contains(c.x) && g.indices.contains(c.y)

  override def partTwo(l: List[String]): Int = {
    def getBasins(
      g: List[List[Int]],
      knownBasinsBySize: List[Int],
      explored: List[Coord],
      currentBasin: List[Coord],
      nextToExplore: List[Coord],
      startCoord: Coord
    ): List[Int] = {
      nextToExplore match {
        case Nil =>
          val maybeNextCoord       = nextCoord(g.size - 1, g.head.size - 1, startCoord)
          val newKnownBasinsBySize = currentBasin.size +: knownBasinsBySize
          maybeNextCoord match {
            case None       => newKnownBasinsBySize
            case Some(next) => getBasins(g, newKnownBasinsBySize, explored, List.empty, List(next), next)
          }
        case firstToExplore :: others if g(firstToExplore.y)(firstToExplore.x) == 9 =>
          getBasins(g, knownBasinsBySize, firstToExplore +: explored, currentBasin, others, startCoord)
        case firstToExplore :: others =>
          val unknownNeighbours = getNeighbours(g, firstToExplore).filterNot(c => (explored ++ nextToExplore).contains(c))
          getBasins(
            g,
            knownBasinsBySize,
            firstToExplore +: explored,
            firstToExplore +: currentBasin,
            others ++ unknownNeighbours,
            startCoord
          )
      }
    }

    def getNeighbours(g: List[List[Int]], c: Coord): List[Coord] =
      List((0, 1), (0, -1), (-1, 0), (1, 0))
        .map { case (xChange, yChange) => Coord(c.x + xChange, c.y + yChange) }
        .filter(isInGrid(_, g))

    def nextCoord(maxY: Int, maxX: Int, c: Coord): Option[Coord] =
      (c.x, c.y) match {
        case (x, y) if x == maxX && y == maxY => None
        case (x, y) if x == maxX              => Some(Coord(0, y + 1))
        case (x, y)                           => Some(Coord(x + 1, y))
      }

    val grid   = l.map(str => str.split("").map(_.toInt).toList)
    val basins = getBasins(grid, Nil, Nil, Nil, List(Coord(0, 0)), Coord(0, 0))
    basins.sorted.reverse.take(3).product
  }
}

object DayNineData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "2199943210",
    "3987894921",
    "9856789892",
    "8767896789",
    "9899965678"
  )
  override val expectedPartOne: Option[Int] = Some(15)
  override val expectedPartTwo: Option[Int] = Some(1134)
}
