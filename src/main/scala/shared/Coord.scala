package shared

case class Coord(x: Int, y: Int) {
  def rotateAroundOrigin(direction: Char): Coord =
    direction match {
      case 'W' => Coord(-y, x)
      case 'E' => Coord(y, -x)
      case 'S' => Coord(-x, -y)
      case 'N' => this
    }

  def move(direction: Char, n: Int): Coord =
    direction match {
      case 'N' => Coord(x, y + n)
      case 'E' => Coord(x + n, y)
      case 'W' => Coord(x - n.toInt, y)
      case 'S' => Coord(x, y - n.toInt)
    }

  def move(xAndYDeltas: (Int, Int)): Coord = this.copy(x + xAndYDeltas._1, y + xAndYDeltas._2)

  def neighbours: Set[Coord] =
    List((-1, -1), (1, -1), (1, 1), (-1, 1), (-1, 0), (0, -1), (0, 1), (1, 0)).map{case (xD, yD) => Coord(x + xD, y+yD)}.toSet

  def taxicabDistanceFromOrigin: Int = x.abs + y.abs
}

object Coord extends Helpers {
  def from(str: String): Coord = {
    val (x, y) = getTwoFromSplit(str, ",")
    Coord(x.toInt, y.toInt)
  }
}