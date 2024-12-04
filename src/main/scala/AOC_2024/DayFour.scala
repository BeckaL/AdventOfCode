package AOC_2024

import shared.{Coord, DayChallenge, GridHelpers, TestData}

object DayFour extends DayChallenge[Int, Int] with GridHelpers {
  override def partOne(l: List[String]): Int = wordsearch(findXmas, 'X', l, adjacentVectors)

  override def partTwo(l: List[String]): Int = wordsearch(findMASCrossed, 'M', l, diagonalVectors) / 2

  private def wordsearch(findFunction: ((Int, Int), Coord, List[String]) => Boolean, startChar: Char, l: List[String], vectors: List[(Int, Int)]): Int =
    l.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.map { case (letter, x) =>
        if (letter == startChar)
          vectors.count(findFunction(_, Coord(x, y), l))
        else 0
      }
    }.sum

  private def findXmas(direction: (Int, Int), coord: Coord, l: List[String]): Boolean =
    getStringFromCoords(l, getCoordsInDirection(coord, direction, 4)) == "XMAS"

  private def findMASCrossed(direction: (Int, Int), coord: Coord, l: List[String]): Boolean =
    val coords = getCoordsInDirection(coord, direction, 3)
    val otherMasCoords = List(Coord(coords.head.x, coords(2).y), Coord(coords(2).x, coords.head.y))
    getStringFromCoords(l, coords) == "MAS" && Set("MS", "SM").contains(getStringFromCoords(l, otherMasCoords))

  private def getStringFromCoords(l: List[String], coords: Seq[Coord]) =
    coords.map(c => if (l.isInGrid(c)) l(c.y)(c.x) else "").mkString("")

  private def getCoordsInDirection(start: Coord, direction: (Int, Int), steps: Int) =
    (0 until steps).map(step => start.move((direction._1 * step, direction._2 * step)))
}

object DayFourData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "MMMSXXMASM",
    "MSAMXMSMSA",
    "AMXSXMAAMM",
    "MSAMASMSMX",
    "XMASAMXAMM",
    "XXAMMXXAMA",
    "SMSMSASXSS",
    "SAXAMASAAA",
    "MAMMMXMMMM",
    "MXMXAXMASX"
  )
  override val expectedPartOne: Option[Int] = Some(18)
  override val expectedPartTwo: Option[Int] = Some(9)
}