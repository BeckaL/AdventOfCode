package AOC_2023

import shared.{Coord, DayChallenge, GridHelpers, TestData}

import scala.annotation.tailrec

object DayThree extends DayChallenge[Int, Long] with GridHelpers {
  override def partOne(l: List[String]): Int =
    val engine = parseEngine(l, 0, 0, emptyEngine)
    engine
      .numbers
      .collect { case (value, coords) if isAdjacentToSymbol(coords, engine.allSymbols) => value }
      .sum

  override def partTwo(l: List[String]): Long =
    val engine = parseEngine(l, 0, 0, emptyEngine)
    engine.gears.map(getGearSum(_, engine.numbers)).sum
    
  private def isAdjacentToSymbol(numberCoords: List[Coord], allSymbols: List[Coord]): Boolean =
    numberCoords.exists(_.neighbours.exists(allSymbols.contains))

  private def getGearSum(gear: Coord, numbers: List[(Int, List[Coord])]): Long =
    val numberNeighbours = numbers.collect { case (n, coords) if gear.neighbours.exists(coords.contains(_)) => n }
    if (numberNeighbours.size == 2) numberNeighbours.map(_.toLong).product else 0L

  @tailrec
  private def parseEngine(l: List[String], x: Int, y: Int, engine: Engine): Engine =
    val (endX, updatedEngine) = l(y)(x) match
      case d if d.isDigit =>
        val n = parseNumber(l, x, y)
        (n._2.map(_.x).max, engine.withNumber(n))
      case '*' => (x, engine.withGear(Coord(x, y)))
      case '.' => (x, engine)
      case _ => (x, engine.withSymbol(Coord(x, y)))
    (endX, y) match
      case (x, y) if x == l.width - 1 && y == l.height - 1 => updatedEngine
      case (x, _) if x == l.width - 1 => parseEngine(l, 0, y + 1, updatedEngine)
      case _ => parseEngine(l, endX + 1, y, updatedEngine)

  private def parseNumber(l: List[String], x: Int, y: Int): (Int, List[Coord]) =
    val numberString = l(y).mkString("").substring(x).takeWhile(_.isDigit)
    val coords = (x until x + numberString.size).toList.map(i => Coord(i, y))
    (numberString.toInt, coords)

  private val emptyEngine = Engine(List(), List(), List())
}

case class Engine(numbers: List[(Int, List[Coord])], gears: List[Coord], otherSymbols: List[Coord]) {
  def allSymbols = gears ++ otherSymbols
  def withNumber(numberAndCoords: (Int, List[Coord])) = this.copy(numbers = numberAndCoords +: numbers)
  def withGear(c: Coord) = this.copy(gears = c +: gears)
  def withSymbol(c: Coord) = this.copy(otherSymbols = c +: otherSymbols)
}

object DayThreeData extends TestData[Int, Long] {
  override val testData: List[String] = List(
    "467..114..",
    "...*......",
    "..35..633.",
    "......#...",
    "617*......",
    ".....+.58.",
    "..592.....",
    "......755.",
    "...$.*....",
    ".664.598.."
  )
  override val expectedPartOne: Option[Int] = Some(4361)
  override val expectedPartTwo: Option[Long] = Some(467835L)
}