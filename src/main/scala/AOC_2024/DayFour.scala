package AOC_2024

import shared.{DayChallenge, TestData, GridHelpers}

object DayFour extends DayChallenge[Int, Int] with GridHelpers {
  override def partOne(l: List[String]): Int =
    l.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.map { case (letter, x) =>
        if (letter == 'X') {
          adjacentVectors.count { case direction => findXmas(direction, (x, y), l) }
        } else 0
      }
    }.sum

  private def findXmas(direction: (Int, Int), coord: (Int, Int), l: List[String]): Boolean =
    val coords = (0 until 4).map(step => (coord._1 + (direction._1 * step), coord._2 + (direction._2 * step)))
    coords.map(c =>
      if (l.isInGrid(c)) Some(l(c._2)(c._1)) else None
    ).flatMap(_.toList).mkString("") == "XMAS"

  override def partTwo(l: List[String]): Int = {
    2
  }
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
  override val expectedPartTwo: Option[Int] = Some(0)
}