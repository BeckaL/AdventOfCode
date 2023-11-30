package shared

trait GridHelpers {
  implicit class StringGridOps(grid: List[String]) {
    def isInGrid(x: Int, y: Int)    = grid.indices.contains(y) && grid.head.indices.contains(x)
    def isInGrid(coord: Coord)      = grid.indices.contains(coord.y) && grid.head.indices.contains(coord.x)
    def isInGrid(xAndY: (Int, Int)) = grid.indices.contains(xAndY._2) && grid.head.indices.contains(xAndY._1)

    def rowAt(y: Int) = grid(y).toList
    def colAt(x: Int) = grid.map(_(x))

    def cellAt(coord: Coord): Char =
      grid(grid.size - 1 - coord.y)(coord.x)

    def height: Int = grid.size

    def width: Int = grid.head.size

    def printable = "\n" + grid.mkString("\n")

    def allCoords = for {
      x <- grid.head.indices
      y <- grid.indices
    } yield Coord(x, y)
  }

  def getImmediateNeighbours(x: Int, y: Int, l: List[String]): List[Char] =
    adjacentVectors.map(v => (x + v._1, y + v._2)).filter { case (x, y) => l.isInGrid(x, y) }.map { case (x, y) => l(y)(x) }

  def getNeighbourCoordNoDiagonals(x: Int, y: Int, l: List[String]): List[(Int, Int)] =
    justAdjacentVectors.map(v => (x + v._1, y + v._2)).filter { case (x, y) => l.isInGrid(x, y) }

  val justAdjacentVectors = List((-1, 0), (0, -1), (0, 1), (1, 0))
  val diagonalVectors = List((-1, -1), (1, -1), (1, 1), (-1, 1))
  val adjacentVectors = diagonalVectors ++ List((-1, 0), (0, -1), (0, 1), (1, 0))

  implicit class IntGridOps(grid: List[List[Int]]) {
    def isInGrid(x: Int, y: Int)    = grid.indices.contains(y) && grid.head.indices.contains(x)
    def isInGrid(coord: Coord)      = grid.indices.contains(coord.y) && grid.head.indices.contains(coord.x)
    def isInGrid(xAndY: (Int, Int)) = grid.indices.contains(xAndY._2) && grid.head.indices.contains(xAndY._1)

    def rowAt(y: Int) = grid(y)
    def colAt(x: Int) = grid.map(_(x))

    def cellAt(coord: Coord): Int =
      grid(grid.size - 1 - coord.y)(coord.x)

    def height: Int = grid.size

    def width: Int = grid.head.size

    def printable = "\n" + grid.mkString("\n")

    def allCoords = for {
      x <- grid.head.indices
      y <- grid.indices
    } yield Coord(x, y)

    def getImmediateNeighbourCoords(c: Coord): List[Coord] =
      adjacentVectors.map(v => Coord(c.x + v._1, c.y + v._2)).filter(isInGrid)

    def getImmediateNeighbours(c: Coord): List[Int] = getImmediateNeighbourCoords(c).map(cellAt)
  }

}
