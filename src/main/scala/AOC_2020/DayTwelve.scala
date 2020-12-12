package AOC_2020

import shared.{DayChallenge, GridHelpers}

object DayTwelve extends DayChallenge[Int, Int] with GridHelpers {
  override def partOne(l: List[String]): Int =
    go(l, State(Coord(0, 0), 'E', Coord(0, 0)), getNewStateWithDirection).taxicabDistanceFromOrigin

  override def partTwo(l: List[String]): Int =
    go(l, State(Coord(0, 0), 'E', Coord(10, 1)), getNewStateWithWayPoint).taxicabDistanceFromOrigin

  @scala.annotation.tailrec
  def go(l: List[String], state: State, newStateFunction: (String, State) => State): Coord =
    if (l.isEmpty) state.position else go(l.tail, newStateFunction(l.head, state), newStateFunction)

  private def getNewStateWithDirection(instruction: String, state: State): State =
    instruction.splitAt(1) match {
      case (instruction, n) if clockWiseDirections.contains(instruction.head) =>
        state.copy(state.position.move(instruction.head, n.toInt))
      case (instruction, n) if List("L", "R").contains(instruction) =>
        state.copy(currentDirection = getNewDirection(instruction, n.toInt, state.currentDirection))
      case ("F", n) => state.copy(state.position.move(state.currentDirection, n.toInt))
    }

  private def getNewStateWithWayPoint(instruction: String, state: State): State = {
    instruction.splitAt(1) match {
      case (instruction, n) if clockWiseDirections.contains(instruction.head) =>
        state.copy(waypoint = state.waypoint.move(instruction.head, n.toInt))
      case (instruction, n) if List("L", "R").contains(instruction) =>
        state.copy(waypoint = rotateWayPoint(n.toInt, instruction, state.waypoint))
      case ("F", n) => state.copy(position = moveToWayPoint(state.waypoint, n.toInt, state.position))
    }
  }

  case class State(position: Coord, currentDirection: Char, waypoint: Coord)

  private def rotateWayPoint(degrees: Int, direction: String, waypoint: Coord): Coord =
    waypoint.rotateAroundOrigin(getNewDirection(direction, degrees, 'N'))

  private def getNewDirection(direction: String, degrees: Int, currentDirection: Char = 'N'): Char =
    direction match {
      case "L" => antiClockWiseDirections((antiClockWiseDirections.indexOf(currentDirection) + degrees / 90) % 4)
      case "R" => clockWiseDirections((clockWiseDirections.indexOf(currentDirection) + degrees / 90) % 4)
    }

  val clockWiseDirections = List('N', 'E', 'S', 'W')
  val antiClockWiseDirections = List('N', 'W', 'S', 'E')

  private def moveToWayPoint(waypoint: Coord, i: Int, currentPosition: Coord): Coord =
    Coord(currentPosition.x + (waypoint.y * i), currentPosition.y + (waypoint.x * i))

  override val testData: List[String] = List("F10",
    "N3",
    "F7",
    "R90",
    "F11")

  override val expectedPartOne: Option[Int] = Some(25)
  override val expectedPartTwo: Option[Int] = Some(286)
}
