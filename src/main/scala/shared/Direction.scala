package shared


object Direction extends Helpers {
  val directions = List('N', 'E', 'S', 'W')

  def rotateClockwise(direction: Char): Char = cycle(directions, direction)

  def reverse(direction: Char): Char = cycle(directions, direction, 2)

  def rotateAntiClockwise(direction: Char): Char = cycle(directions, direction, 3)
}
