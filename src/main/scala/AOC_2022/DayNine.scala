package AOC_2022

import shared.{DayChallenge, Helpers, TestData, Coord}

object DayNine extends DayChallenge[Int, Int] with Helpers {
  override def partOne(l: List[String]): Int =
    coordsVisitedByLastKnot(startKnots = List(Coord(0, 0)), instructions = parse(l)).size

  override def partTwo(l: List[String]): Int = {
    val startKnots = (0 until 9).toList.map(_ => Coord(0, 0))
    coordsVisitedByLastKnot(startKnots = startKnots, instructions = parse(l)).size
  }

  private def coordsVisitedByLastKnot(startKnots: List[Coord], instructions: List[(String, Int)]): Set[Coord] =
    instructions.foldLeft(Coord(0, 0), startKnots, Set[Coord]()) { case ((head, knots, visited), (dir, i)) =>
      val (newHead, newKnots, newVisited) = moveNTimes(head, knots, i, dir)
      (newHead, newKnots, visited ++ newVisited)
    }._3

  private def maybeMoveTail(head: Coord, tail: Coord): Coord =
    if (tail.neighbours.contains(head))
      tail
    else
      Coord(moveTowards(tail.x, head.x), moveTowards(tail.y, head.y))

  private def moveTowards(from: Int, to: Int): Int =
    to - from match {
      case 0 => from
      case i if i < 0 => from - 1
      case _ => from + 1
    }

  private def moveAllKnotsTowardsHead(head: Coord, tails: List[Coord]): List[Coord] =
    tails.foldLeft(List[Coord]()) { case (newTails, tailToMove) =>
      val newT = newTails match {
        case Nil => maybeMoveTail(head, tailToMove)
        case nonEmptyMovedTails@_ => maybeMoveTail(nonEmptyMovedTails.last, tailToMove)
      }
      newTails :+ newT
    }

  private def moveNTimes(head: Coord, tails: List[Coord], n: Int, dir: String): (Coord, List[Coord], Set[Coord]) =
    (0 until n).foldLeft((head, tails, Set[Coord]())) { case ((h, t, knot9visits), _) =>
      val newH = h.move(dirsToCompassDirs(dir), 1)
      val newTails = moveAllKnotsTowardsHead(newH, t)
      (newH, newTails, knot9visits + newTails.last)
    }

  private def parse(l: List[String]): List[(String, Int)] = l.map(splitToStringAndInt(_, " "))

  val dirsToCompassDirs = Map("R" -> 'E', "L" -> 'W', "U" -> 'N', "D" -> 'S')
}

object DayNineData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "R 4",
    "U 4",
    "L 3",
    "D 1",
    "R 4",
    "D 1",
    "L 5",
    "R 2"
  )

  override val testData2: Option[List[String]] = Some(List(
    "R 5",
    "U 8",
    "L 8",
    "D 3",
    "R 17",
    "D 10",
    "L 25",
    "U 20"
  ))
  override val expectedPartOne: Option[Int] = Some(13)
  override val expectedPartTwo: Option[Int] = Some(36)
}

