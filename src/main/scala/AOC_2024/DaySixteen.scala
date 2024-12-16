package AOC_2024

import shared.{Coord, DayChallenge, GridHelpers, TestData, dijkstra}

object DaySixteen extends DayChallenge[Int, Int] with GridHelpers {
  override def partOne(input: List[String]): Int =
    val start = Node(input.allCoords.find(input.get(_) == 'S').get, 'E', 0)
    val target = input.allCoords.find(input.get(_) == 'E').get
    val blankInput = input.map(_.replaceAll("E", ".").replaceAll("S", "."))
    val nextNodeFunction = nextNodes(blankInput)
    dijkstra[Node](getWeight, nextNodes(blankInput), isTarget(target), Set(start))

  private def getWeight(n: Node, unusedInt: Int = 0) = n.score

  case class Node(coord: Coord, direction: Char, score: Int)

  private def isTarget(target: Coord)(n: Node) = n.coord == target

  def nextNodes(grid: List[String])(node: Node, visited: Set[Node]): List[Node] =
    val possibleDirections = node.direction match
      case 'E' => List('N', 'S', 'E')
      case 'W' => List('N', 'S', 'W')
      case 'S' => List('E', 'W', 'S')
      case 'N' => List('E', 'W', 'N')
    possibleDirections
      .map(d => d -> node.coord.move(d, 1))
      .map { case (newDirection, coord) =>
        val newScore = if (newDirection == node.direction) node.score + 1 else node.score + 1001
        Node(coord, newDirection, newScore)
      }.filter(node => grid.isInGrid(node.coord) && grid.get(node.coord) == '.' &&
        !visited.exists(aNode =>
          aNode.direction == node.direction && aNode.coord == node.coord && aNode.score < node.score)
      )

  override def partTwo(input: List[String]): Int =
    ???
}

object DaySixteenData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "###############",
    "#.......#....E#",
    "#.#.###.#.###.#",
    "#.....#.#...#.#",
    "#.###.#####.#.#",
    "#.#.#.......#.#",
    "#.#.#####.###.#",
    "#...........#.#",
    "###.#.#####.#.#",
    "#...#.....#.#.#",
    "#.#.#.###.#.#.#",
    "#.....#...#.#.#",
    "#.###.#.#.#.#.#",
    "#S..#.....#...#",
    "###############"
  )
  override val expectedPartOne: Option[Int] = Some(7036)
  override val expectedPartTwo: Option[Int] = Some(64)
}