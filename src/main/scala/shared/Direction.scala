package shared


object Direction extends Helpers {
  val directions = List('N', 'E', 'S', 'W')

  def rotateClockwise(direction: Char): Char = cycle(directions, direction)

  def reverse(direction: Char): Char = cycle(directions, direction, 2)

  def rotateAntiClockwise(direction: Char): Char = cycle(directions, direction, 3)

  def directionFromPictoral(i: Char) =
    i match 
      case '^' => 'N'
      case 'v' => 'S'
      case '>' => 'E'
      case '<' => 'W'
      case char => throw new RuntimeException(s"didn't understand char $char needed to be one of < > ^ v")
}
