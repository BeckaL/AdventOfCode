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

  def moveYIsReversed(direction: Char, n: Int): Coord =
    direction match {
      case 'N' => Coord(x, y - n)
      case 'E' => Coord(x + n, y)
      case 'W' => Coord(x - n.toInt, y)
      case 'S' => Coord(x, y + n.toInt)
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
  def from(xAndY: (Int, Int)) = {
    Coord(xAndY._1, xAndY._2)
  }
}


case class Coord3D(x: Int, y: Int, z: Int) {
  def manhattanDistanceToOrigin = x.abs + y.abs + z.abs
  def neighbours: List[Coord3D] = List((-1, 0, 0), (1, 0, 0), (0, -1, 0), (0, 1, 0), (0, 0, -1), (0, 0, 1)).map{
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