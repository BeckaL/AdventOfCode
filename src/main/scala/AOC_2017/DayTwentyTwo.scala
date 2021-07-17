package AOC_2017

import shared.{Coord, DayChallenge, Direction, GridHelpers, Helpers, TestData}

object DayTwentyTwo extends DayChallenge[Int, Int] with GridHelpers with Helpers {

  override def partOne(l: List[String]): Int = {
    val stateToggler = (current: Char) =>  if (current == '#') '.' else '#'
    getNumberOfInfections(l, 10000, '.', stateToggler)
  }

  override def partTwo(l: List[String]): Int = {
    val requiredIterations = if(l == DayTwentyTwoData.testData) 100 else 10000000
    val stateToggler  = (current: Char) => cycle(List('.', 'W', '#', 'F'), current)
    getNumberOfInfections(l, requiredIterations, 'W', stateToggler)
  }

  private def getNumberOfInfections(
                   grid: List[String],
                   requiredIterations: Int,
                   infectionWarning: Char,
                   stateToggler: Char => Char,
                 ): Int = {

    def updateState(c: Coord, grid: List[String], d: Char, infections: Int): (List[String], Coord, Char, Int) = {
      val currentCell = grid.cellAt(c)
      val newInfections = if (currentCell == infectionWarning) infections + 1 else infections
      val gridWithNewInfection = updateGridAt(c, stateToggler(currentCell), grid)
      val newDirection = currentCell match {
        case '.' => Direction.rotateAntiClockwise(d)
        case '#' => Direction.rotateClockwise(d)
        case 'W' => d
        case 'F' => Direction.reverse(d)
      }
      val (updatedCoord, newGridAfterNewCoord) = getNewGridAndUpdatedCoord(c.move(newDirection, 1), gridWithNewInfection)
      (newGridAfterNewCoord, updatedCoord, newDirection, newInfections)
    }

    val startingState = (grid, Coord(grid.size / 2, grid.head.size / 2), 'N', 0)
    val (_, _, _, infections) = (1 to requiredIterations).foldLeft(startingState){
        case ((currentGrid, currentCoord, currentDirection, currentInfections), _) =>
          updateState(currentCoord, currentGrid, currentDirection, currentInfections)
      }
    infections
  }

  private def getNewGridAndUpdatedCoord(c: Coord, g: List[String]): (Coord, List[String]) =
    c match {
      case _ if g.isInGrid(c)          => (c, g)
      case Coord(x, _) if x < 0        => (c.copy(x = x + 1), g.map(r => "." + r))
      case Coord(x, _) if x >= g.width => (c, g.map(r => r + "."))
      case Coord(_, y) if y < 0        => (c.copy(y = y + 1), g :+ "." * g.width)
      case _                           => (c, "." * g.width +: g)
    }

  private def updateGridAt(cell: Coord, char: Char, grid: List[String]) = {
    val yIndexOfReverse = grid.size - 1 - cell.y
    grid.updated(yIndexOfReverse, grid(yIndexOfReverse).updated(cell.x, char))
  }
}

object DayTwentyTwoData extends TestData[Int, Int] {
  override val testData: List[String] = List("..#", "#..", "...")
  override val expectedPartOne: Option[Int] = Some(5587)
  override val expectedPartTwo: Option[Int] = None
}