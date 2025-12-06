package AOC_2025

import shared.{DayChallenge, TestData}

import scala.annotation.tailrec

object DaySix extends DayChallenge[Long, Long] {
  override def partOne(l: List[String]): Long =
    val rows = l.map(_.trim.split("\\s+").toList)
    performOperations(rows).sum

  private def performOperations(lines: List[List[String]]) =
    val columns = lines.head.indices.map(i => lines.map(line => line(i))).toList
    columns.map(col => performOperationsOnColumn(col.dropRight(1), col.last))

  @tailrec
  private def performOperationsOnColumn(col: List[String], op: String, soFar: Long = 0): Long =
    col match
      case Nil => soFar
      case first :: others =>
        val newRunningTotal = op match
          case "*" =>
            first.toLong * (if (soFar == 0) 1 else soFar)
          case "+" => first.toLong + soFar
        performOperationsOnColumn(others, op, newRunningTotal)

  @tailrec
  private def getNextSetOfNumbers(rows: List[String], i: Int, soFar: List[Long]): (List[Long], Int) =
    rows.map(_(i)).filterNot(_ == ' ').mkString match
      case "" => (soFar, i - 1)
      case _ if i == 0 => (soFar, i)
      case n => getNextSetOfNumbers(rows, i - 1, soFar :+ n.toLong)

  @tailrec
  private def performSums(operators: List[String], rows: List[String], i: Int, soFar: List[Long]): List[Long] =
    operators match
      case Nil => soFar
      case op :: remainingOps =>
        val (numbers, newI) = getNextSetOfNumbers(rows, i, List())
        val newNumber = op match
          case "*" =>
            numbers.foldLeft(1L) { case (resultSoFar, n) => resultSoFar * n }
          case "+" => numbers.foldLeft(0L) { case (resultSoFar, n) => resultSoFar + n }
        performSums(remainingOps, rows, newI, soFar :+ newNumber)

  override def partTwo(l: List[String]): Long =
    val operatorsInReverseOrder = l.last.trim.split("\\s+").toList.reverse
    performSums(operatorsInReverseOrder, l.dropRight(1), l.head.indices.max, List()).sum
}

object DaySixData extends TestData[Long, Long] {
  override val testData: List[String] = List(
    "123 328  51 64 ",
    " 45 64  387 23 ",
    "  6 98  215 314",
    "*   +   *   +  "
  )
  override val expectedPartOne: Option[Long] = Some(4277556)
  override val expectedPartTwo: Option[Long] = Some(3263827)
}