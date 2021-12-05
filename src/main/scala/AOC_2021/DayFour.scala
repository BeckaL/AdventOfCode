package AOC_2021

import shared.{DayChallenge, TestData}

import scala.annotation.tailrec

object DayFour extends DayChallenge[Int, Int] {
  override def partOne(l: List[String]): Int = {
    val (lastNumberCalled, winningBoard) = findWinner(getNumbersCalled(l), getBoards(l))
    winningBoard.sumOfUnmarked * lastNumberCalled
  }

  @tailrec
  def findWinner(numbers: List[Int], boards: List[BingoBoard]): (Int, BingoBoard) =
    numbers match {
      case n :: others =>
        val newBoards = boards.map(b => b.registerNumber(n))
        newBoards.find(b => b.wins) match {
          case Some(winner) => (n, winner)
          case _            => findWinner(others, newBoards)
        }
      case _ => throw new RuntimeException("Didn't get a winner")
    }

  override def partTwo(l: List[String]): Int = {
    val (lastNumberCalled, losingBoard) = findLoser(getNumbersCalled(l), getBoards(l))
    losingBoard.sumOfUnmarked * lastNumberCalled
  }

  @tailrec
  def findLoser(numbers: List[Int], boards: List[BingoBoard]): (Int, BingoBoard) =
    numbers match {
      case n :: others =>
        val newBoards = boards.map(b => b.registerNumber(n))
        newBoards match {
          case singleBoard :: Nil if singleBoard.wins => (n, singleBoard)
          case singleBoad :: Nil                      => findLoser(others, List(singleBoad))
          case _                                      => findLoser(others, newBoards.filterNot(b => b.wins))
        }
      case _ => throw new RuntimeException("Didn't get a winner")
    }

  private def getNumbersCalled(l: List[String]): List[Int] =
    l.head.split(",").map(_.toInt).toList

  private def getBoards(l: List[String]): List[BingoBoard] =
    l.tail.grouped(6)
      .map(boardWithSpace =>
        BingoBoard(boardWithSpace.tail.map(rowWithNumbers =>
          rowWithNumbers.trim.split("\\s+").toList.map(str => Cell(str.toInt, false))
        ))
      ).toList
}

case class Cell(number: Int, isMarked: Boolean)

case class BingoBoard(rs: List[List[Cell]]) {
  val columns = (0 until 5).toList.map { columnIndex => rs.map(_(columnIndex)) }
  def wins =
    rs.exists(_.forall(_.isMarked)) || columns.exists(_.forall(_.isMarked))

  def registerNumber(n: Int) =
    BingoBoard(rs.map { row => row.map { cell => if (cell.number == n) cell.copy(isMarked = true) else cell } })

  def sumOfUnmarked = rs.flatten.collect { case c if !c.isMarked => c.number }.sum
}

object DayFourData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1",
    "      ",
    "22 13 17 11  0",
    " 8  2 23  4 24",
    "21  9 14 16  7",
    " 6 10  3 18  5",
    " 1 12 20 15 19",
    "      ",
    " 3 15  0  2 22",
    " 9 18 13 17  5",
    "19  8  7 25 23",
    "20 11 10 24  4",
    "14 21 16 12  6",
    "      ",
    "14 21 17 24  4",
    "10 16 15  9 19",
    "18  8 23 26 20",
    "22 11 13  6  5",
    " 2  0 12  3  7"
  )
  override val expectedPartOne: Option[Int] = Some(4512)
  override val expectedPartTwo: Option[Int] = Some(1924)
}
