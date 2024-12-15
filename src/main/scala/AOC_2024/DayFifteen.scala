package AOC_2024

import shared.Direction.directionFromPictoral
import shared.{Coord, DayChallenge, GridHelpers, Helpers, TestData}

import scala.annotation.tailrec

object DayFifteen extends DayChallenge[Int, Int] with Helpers with GridHelpers {
  override def partOne(input: List[String]): Int =
    getEndBoxes(input, getBoxesPartOne).map(_.head).map(b => b.x + b.y * 100).sum

  override def partTwo(input: List[String]): Int =
    val scaleGrid = (g: List[String]) => g.map(row => row.map {
      case '@' => "@."
      case 'O' => "[]"
      case otherChar => otherChar.toString * 2
    }.mkString(""))
    getEndBoxes(input, getBoxesPartTwo, scaleGrid).map(_.head).map(b => b.x + b.y * 100).sum

  private def getEndBoxes(l: List[String], boxesFinder: BoxesFinder, gridTransformer: GridTransformer = identity) =
    val (bs, r, walls, instructions) = parse(l, boxesFinder, gridTransformer)
    instructions.foldLeft((bs, r)) { case ((boxes, robot), dir) =>
      val robotIfMoved = robot.moveYIsReversed(dir, 1)
      if (walls.contains(robotIfMoved))
        (boxes, robot)
      else
        boxes.find(_.contains(robotIfMoved)) match
          case Some(firstToMove) => shuffleBoxes(dir, boxes, walls, robot, firstToMove)
          case None => (boxes, robotIfMoved)
    }._1

  private def boxesToMove(dir: Char, allBoxes: List[Box], first: Box, walls: Box): List[Box] =
    @tailrec
    def go(soFar: List[Box], next: List[Box]): List[Box] =
      next match
        case Nil => soFar
        case first :: others =>
          val coordsIfMoved = first.map(_.moveYIsReversed(dir, 1))
          if (walls.exists(coordsIfMoved.contains))
            List.empty
          else
            allBoxes.filter(_.exists(coordsIfMoved.contains)) match
              case Nil => go(soFar :+ first, others)
              case nextBoxes => go(soFar :+ first, (others ++ nextBoxes).filterNot(soFar.contains))

    go(List(), List(first))

  private def shuffleBoxes(dir: Char, boxes: List[Box], walls: Box, robot: Coord, first: Box) =
    val totalBoxesToMove = boxesToMove(dir, boxes, first, walls)
    if (totalBoxesToMove.isEmpty)
      (boxes, robot)
    else
      val movedBoxes = boxes.map(b => if (totalBoxesToMove.contains(b)) b.map(_.moveYIsReversed(dir, 1)) else b)
      (movedBoxes, robot.moveYIsReversed(dir, 1))

  private type GridTransformer = List[String] => List[String]
  private type Box = List[Coord]
  private type BoxesFinder = List[String] => List[Box]

  private def parse(input: List[String], boxesFinder: BoxesFinder, gridTransformer: GridTransformer = identity) =
    val groups = splitIntoGroupsOfList(input)
    val g = gridTransformer(groups.head)
    val robot = g.coordsWhereCharIs(_ == '@').head
    val walls = g.coordsWhereCharIs(_ == '#')
    val dirs = groups(1).mkString("").map(directionFromPictoral)
    (boxesFinder(g), robot, walls, dirs)

  private def getBoxesPartOne(g: List[String]) = g.coordsWhereCharIs(_ == 'O').map(List(_))
  private def getBoxesPartTwo(g: List[String]) = g.coordsWhereCharIs(_ == '[').map(c => List(c, c.copy(c.x + 1)))
}

object DayFifteenData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "##########",
    "#..O..O.O#",
    "#......O.#",
    "#.OO..O.O#",
    "#..O@..O.#",
    "#O#..O...#",
    "#O..O..O.#",
    "#.OO.O.OO#",
    "#....O...#",
    "##########",
    "",
    "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^",
    "vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v",
    "><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<" +
      "<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^" +
      "^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><" +
      "^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^" +
      ">^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^" +
      "<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>" +
      "^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>" +
      "v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"
  )
  override val expectedPartOne: Option[Int] = Some(10092)
  override val expectedPartTwo: Option[Int] = Some(9021)
}