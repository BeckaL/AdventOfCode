package shared

case class Coord(x: Int, y: Int) {
  def rotateAroundOrigin(direction: Char): Coord =
    direction match {
      case 'W' => Coord(-y, x)
      case 'E' => Coord(y, -x)
      case 'S' => Coord(-x, -y)
      case 'N' => this
    }

  def moveYStartsFromOrigin(direction: Char, n: Int): Coord =
    direction match {
      case 'N' => Coord(x, y + n)
      case 'E' => Coord(x + n, y)
      case 'W' => Coord(x - n, y)
      case 'S' => Coord(x, y - n)
    }

  def move(direction: Char, n: Int): Coord =
    direction match {
      case 'N' => Coord(x, y - n)
      case 'E' => Coord(x + n, y)
      case 'W' => Coord(x - n, y)
      case 'S' => Coord(x, y + n)
    }

  def move(xAndYDeltas: (Int, Int)): Coord = this.copy(x + xAndYDeltas._1, y + xAndYDeltas._2)

  def neighbours: Set[Coord] =
    List((-1, -1), (1, -1), (1, 1), (-1, 1), (-1, 0), (0, -1), (0, 1), (1, 0)).map { case (xD, yD) => Coord(x + xD, y + yD) }.toSet

  def neighboursWithoutDiagonals: Set[Coord] =
    List((-1, 0), (0, -1), (0, 1), (1, 0)).map { case (xD, yD) => Coord(x + xD, y + yD) }.toSet
  def taxicabDistanceFromOrigin: Int = x.abs + y.abs
  
  def manhattanDistanceFrom(c2: Coord): Int = (x - c2.x).abs + (y - c2.y).abs

  def nextCoordInGrid(g: List[String]): Option[Coord] =
    if (x == g.head.length - 1)
      if (y == g.length - 1)
        None
      else
        Some(Coord(0, y + 1))
    else
      Some(Coord(x + 1, y))
}

object Coord extends Helpers {
  def from(str: String): Coord = {
    val (x, y) = getTwoFromSplit(str, ",")
    Coord(x.toInt, y.toInt)
  }

  def from(xAndY: (Int, Int)) = {
    Coord(xAndY._1, xAndY._2)
  }
  
  def origin = Coord(0, 0)
}


case class Coord3D(x: Int, y: Int, z: Int) {
  def manhattanDistanceToOrigin = x.abs + y.abs + z.abs

  def neighbours: List[Coord3D] = List((-1, 0, 0), (1, 0, 0), (0, -1, 0), (0, 1, 0), (0, 0, -1), (0, 0, 1)).map {
    case (xD, yD, zD) => Coord3D(x + xD, y + yD, z + zD)
  }
}

object Coord3D {
  def from(list: List[Int]): Coord3D = {
    list match {
      case x :: y :: z :: _ => Coord3D(x, y, z)
      case _ => throw new RuntimeException(s"Needed three elements to make coord 3d, got $list")
    }
  }
}