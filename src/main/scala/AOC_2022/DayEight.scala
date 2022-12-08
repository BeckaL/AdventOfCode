package AOC_2022

import shared.{DayChallenge, TestData}

object DayEight extends DayChallenge[Int, Int] {
  override def partOne(l: List[String]): Int = {
    val rows = l.map(_.split("").map(_.toInt).toList)
    val numberOfEdges = l.head.size * 2 + (l.size - 2) * 2
    insideCoords(rows).count{case (x, y) => isVisible(x, y, rows)} + numberOfEdges
  }

  private def isVisible(x: Int, y: Int, rows: List[List[Int]]): Boolean = {
    val row = rows(y)
    val column = rows.map(_(x))
    row.slice(0, x).forall(_ < row(x)) ||
      row.slice(x + 1, row.size).forall(_ < row(x)) ||
      column.slice(0, y).forall(_ < column(y)) ||
      column.slice(y + 1, column.size).forall(_ < column(y))
  }

  private def insideCoords(rows: List[List[Int]]): List[(Int, Int)] =
    for {
      x <- (1 until rows.head.size - 1).toList
      y <- (1 until rows.size - 1).toList
    } yield (x, y)

  override def partTwo(l: List[String]): Int = {
    val rows = l.map(_.split("").map(_.toInt).toList)
    insideCoords(rows).map{case (x, y) => scenicScore(x, y, rows)}.max
  }

  private def scenicScore(x: Int, y: Int, rows: List[List[Int]]): Int = {
    val value = rows(y)(x)
    val row = rows(y)
    val column = rows.map(_(x))
    val left = row.slice(0, x).reverse
    val right = row.slice(x + 1, rows.size)
    val top = column.slice(0, y).reverse
    val bottom = column.slice(y + 1, column.size)

    def countUntilBlocked(orderedRow: List[Int], count: Int = 0): Int =
      orderedRow match {
        case head :: tail => if (head >= value) count + 1 else countUntilBlocked(tail, count + 1)
        case Nil => count
      }

    List(left, right, top, bottom).map(countUntilBlocked(_)).product
  }
}

object DayEightData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "30373",
    "25512",
    "65332",
    "33549",
    "35390"
  )
  override val expectedPartOne: Option[Int] = Some(21)
  override val expectedPartTwo: Option[Int] = Some(8)
}