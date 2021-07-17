package shared

trait GridHelpers {
  implicit class GridOps(grid: List[String]) {
    def isInGrid(x: Int, y: Int) = grid.indices.contains(y) && grid.head.indices.contains(x)
    def isInGrid(coord: Coord) = grid.indices.contains(coord.y) && grid.head.indices.contains(coord.x)
    def isInGrid(xAndY: (Int, Int)) = grid.indices.contains(xAndY._2) && grid.head.indices.contains(xAndY._1)

    def rowAt(y: Int) = grid(y).toList
    def colAt(x: Int) = grid.map(_(x))

    def cellAt(coord: Coord): Char =
      grid(grid.size - 1 - coord.y)(coord.x)

    def height: Int = grid.size

    def width: Int = grid.head.size

    def printable = "\n" + grid.mkString("\n")
  }

  def getImmediateNeighbours(x: Int, y: Int, l: List[String]): List[Char] =
    adjacentVectors.map(v => (x + v._1, y + v._2)).filter{case (x, y) => l.isInGrid(x, y)}.map { case (x, y) => l(y)(x) }

  val diagonalVectors = List((-1, -1), (1, -1), (1, 1), (-1, 1))
  val adjacentVectors = diagonalVectors ++ List((-1, 0), (0, -1), (0, 1), (1, 0))
}
