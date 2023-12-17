package AOC_2023

import shared.{Coord, DayChallenge, GridHelpers, TestData}

object DaySixteen extends DayChallenge[Int, Int] with GridHelpers{
  override def partOne(l: List[String]): Int = countEnergised(Set(Beam(Coord(-1, 0), 'E')), l)

  override def partTwo(l: List[String]): Int = findMaxEnergised(l)

  case class Beam(position: Coord, direction: Char)

  private def countEnergised(beams: Set[Beam], g: List[String], visited: Set[Beam] = Set.empty): Int =
    if (beams.isEmpty )
      visited.map(_.position).size
    else
      val newBeams = beams.flatMap(b => move(b, g)).diff(visited)
      countEnergised(newBeams, g, visited ++ newBeams)

  private def findMaxEnergised(g: List[String]): Int =
    val possibleNorthStarts = g.head.indices.map(x => Beam(Coord(x, -1), 'S'))
    val possibleSouthStarts = g.head.indices.map(x => Beam(Coord(x, g.size), 'N'))
    val possibleWestStarts = g.indices.map(y => Beam(Coord(-1, y), 'E'))
    val possibleEastStarts = g.indices.map(y => Beam(Coord(g.head.size, y), 'W'))
    (possibleNorthStarts ++ possibleSouthStarts ++ possibleEastStarts ++ possibleWestStarts).foldLeft(0){ case (currentMax, startBeam) =>
      val result = countEnergised(Set(startBeam), g)
      if (result > currentMax) result else currentMax
    }

  private val forwardSlashTranslations = Map('E' -> 'N', 'N' -> 'E', 'W' -> 'S', 'S' -> 'W')
  private val backSlashTranslations = Map('E' -> 'S', 'S' -> 'E', 'W' -> 'N', 'N' -> 'W')
  private def move(beam: Beam, g: List[String]): Set[Beam] =
    val newCoord = beam.position.moveYIsReversed(beam.direction, 1)
    if (g.isInGrid(newCoord)) {
      val next = g(newCoord.y)(newCoord.x)
      val newBeam = beam.copy(position = newCoord)
      (next, beam.direction) match
        case ('|', d) if Set('N', 'S').contains(d) => Set(newBeam)
        case ('|', _) => Set(newBeam.copy(direction = 'N'), newBeam.copy(direction = 'S'))
        case ('-', d) if Set('E', 'W').contains(d) => Set(newBeam)
        case ('-', _) => Set(newBeam.copy(direction = 'W'), newBeam.copy(direction = 'E'))
        case ('/', currentDirection) =>
          Set(newBeam.copy(direction = forwardSlashTranslations(currentDirection)))
        case ('\\', currentDirection) =>
          Set(newBeam.copy(direction = backSlashTranslations(currentDirection)))
        case _ => Set(newBeam)
    } else Set.empty
}

object DaySixteenData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    ".|...\\....",
      "|.-.\\.....",
      ".....|-...",
      "........|.",
      "..........",
      ".........\\",
      "..../.\\\\..",
      ".-.-/..|..",
      ".|....-|.\\",
      "..//.|...."
  )
  override val expectedPartOne: Option[Int] = Some(46)
  override val expectedPartTwo: Option[Int] = Some(51)
}