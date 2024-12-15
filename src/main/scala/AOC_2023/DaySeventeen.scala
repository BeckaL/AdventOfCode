package AOC_2023

import shared.{Coord, DayChallenge, GridHelpers, TestData, dijkstra}

import scala.annotation.tailrec

object DaySeventeen extends DayChallenge[Int, Int] with GridHelpers {
  override def partOne(l: List[String]): Int =
    dijkstra(getWeight(l), nextNodes(l, 0, 3), isTarget(l.maxCoord), startNodes)

  override def partTwo(l: List[String]): Int =
    dijkstra(getWeight(l), nextNodes(l, 3, 10), isTarget(l.maxCoord), startNodes)

  private val startNodes = Set(Node(Coord.origin, 'E', 0), Node(Coord.origin, 'S', 0))

  case class Node(coord: Coord, direction: Char, steps: Int)

  private def isTarget(target: Coord)(n: Node) = n.coord == target

  private def getWeight(g: List[String])(node: Node, currentWeight: Int) =
    g(node.coord.y)(node.coord.x).toString.toInt + currentWeight

  def nextNodes(grid: List[String], min: Int, max: Int)(node: Node, visited: Set[Node]): List[Node] =
    val possibleDirections = (node.direction, node.steps) match
      case (_, steps) if steps < min => List(node.direction)
      case ('E', _) => List('N', 'S', 'E')
      case ('W', _) => List('N', 'S', 'W')
      case ('S', _) => List('E', 'S', 'W')
      case ('N', _) => List('E', 'N', 'W')
    possibleDirections
      .map(d => d -> node.coord.move(d, 1))
      .map { case (newDirection, coord) =>
        Node(coord, newDirection, if (newDirection == node.direction) node.steps + 1 else 0)
      }.filter(node => grid.isInGrid(node.coord) && node.steps < max && !visited.contains(node))
}

object DaySeventeenData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "2413432311323",
    "3215453535623",
    "3255245654254",
    "3446585845452",
    "4546657867536",
    "1438598798454",
    "4457876987766",
    "3637877979653",
    "4654967986887",
    "4564679986453",
    "1224686865563",
    "2546548887735",
    "4322674655533"
  )
  override val expectedPartOne: Option[Int] = Some(102)
  override val expectedPartTwo: Option[Int] = Some(94)
}