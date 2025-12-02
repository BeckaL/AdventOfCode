package AOC_2025

import shared.{DayChallenge, TestData}

import scala.annotation.tailrec

object DayOne extends DayChallenge[Int, Int] {
  override def partOne(l: List[String]): Int =
    val instructions = l.map(s => (s.head, s.tail.toInt))
    getValues(50, instructions, List()).count(_ == 0)

  @tailrec
  private def getValues(current: Int, instructions: List[(Char, Int)], soFar: List[Int]): List[Int] =
    instructions match
      case Nil => soFar
      case first :: others =>
        val turnsModulo100 = first._2 % 100
        val addition = if (first._1 == 'L') 100 - turnsModulo100 else turnsModulo100
        val newCurrent = (current + addition) % 100
        getValues(newCurrent, others, soFar :+ newCurrent)

  @tailrec
  private def rotate(current: Int, clicks: Int, dir: Char, visited: List[Int]): List[Int] =
    if (clicks == 0)
      visited
    else
      val newCurrent = (dir, current) match {
        case ('L', 0) => 99
        case ('L', i) => i - 1
        case ('R', 99) => 0
        case (_, i) => i + 1
      }
      rotate(newCurrent, clicks - 1, dir, visited :+ newCurrent)

  @tailrec
  private def countClicksAt0(current: Int, instructions: List[(Char, Int)], soFar: Int): Int =
    instructions match
      case Nil => soFar
      case first :: others =>
        val visited = rotate(current, first._2, first._1, List())
        val newSoFar = soFar + visited.count(_ == 0)
        countClicksAt0(visited.last, others, newSoFar)


  override def partTwo(l: List[String]): Int =
    val instructions = l.map(s => (s.head, s.tail.toInt))
    countClicksAt0(50, instructions, 0)
}

object DayOneData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "L68",
    "L30",
    "R48",
    "L5",
    "R60",
    "L55",
    "L1",
    "L99",
    "R14",
    "L82"
  )
  override val expectedPartOne: Option[Int] = Some(3)
  override val expectedPartTwo: Option[Int] = Some(6)
}