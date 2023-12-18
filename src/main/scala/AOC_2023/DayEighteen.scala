package AOC_2023

import shared.{Coord, DayChallenge, GridHelpers, TestData}

import scala.annotation.tailrec

object DayEighteen extends DayChallenge[Int, Long] with GridHelpers {
  override def partOne(l: List[String]): Int =
    val instructions = l.map { line =>
      val split = line.split(" ")
      (split(0)(0), split(1).toInt)
    }
    picksArea(instructions).toInt

  private def populateGrid(loop: List[Coord]) =
    (loop.minBy(_.y).y to loop.maxBy(_.y).y).map(y =>
      (loop.minBy(_.x).x to loop.maxBy(_.x).x).map(x =>
        if (loop.contains(Coord(x, y))) '#' else '.'
      ).mkString("")
    ).toList

  private def getCornerPoints(instructions: List[(Char, Int)]) =
    instructions.foldLeft(List(Coord(0, 0))) { case (coordsSoFar, (direction, steps)) =>
      val compassDirection = Map('U' -> 'N', 'D' -> 'S', 'R' -> 'E', 'L' -> 'W')(direction)
      coordsSoFar.head.move(compassDirection, steps) +: coordsSoFar
    }

  private def picksArea(instructions: List[(Char, Int)]) =
    val boundarySize = instructions.map(_._2).sum
    getAreaShoelace(getCornerPoints(instructions)) + (boundarySize / 2) + 1

  private def getAreaShoelace(pointsInOrder: List[Coord]) =
    val xs = pointsInOrder.map(_.x)
    val ys = pointsInOrder.map(_.y)
    val getShoelaceSum = (is: List[Int], js: List[Int]) => is.zip(js.drop(1) :+ js.head).map { case (i1, j2) => i1.toLong * j2.toLong }.sum
    val underlace = ys.dropRight(1).zip(xs.drop(1) :+ xs.head)
    (getShoelaceSum(xs, ys) - getShoelaceSum(ys, xs)).abs / 2

  override def partTwo(l: List[String]): Long =
    val instructions = l.map(line =>
      val instructionString = line.split(" ").last.replaceAll("(\\(|\\))", "").tail
      val direction = Map('0' -> 'R', '1' -> 'D', '2' -> 'L', '3' -> 'U')(instructionString.last)
      val number = Integer.parseInt(instructionString.dropRight(1), 16)
      (direction, number)
    )
    picksArea(instructions)
}

object DayEighteenData extends TestData[Int, Long] {
  override val testData: List[String] = List(
    "R 6 (#70c710)",
    "D 5 (#0dc571)",
    "L 2 (#5713f0)",
    "D 2 (#d2c081)",
    "R 2 (#59c680)",
    "D 2 (#411b91)",
    "L 5 (#8ceee2)",
    "U 2 (#caa173)",
    "L 1 (#1b58a2)",
    "U 2 (#caa171)",
    "R 2 (#7807d2)",
    "U 3 (#a77fa3)",
    "L 2 (#015232)",
    "U 2 (#7a21e3)"
  )
  override val expectedPartOne: Option[Int] = Some(62)
  override val expectedPartTwo: Option[Long] = Some(952408144115L)
}