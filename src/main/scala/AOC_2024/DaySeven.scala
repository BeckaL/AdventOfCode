package AOC_2024

import shared.{DayChallenge, Helpers, TestData, Itertools}

object DaySeven extends DayChallenge[Long, Long] with Helpers {
  override def partOne(l: List[String]): Long =
    l.map(extractLongs).collect {
      case is if isPossible(is.head, is.tail, List("+", "*")) => is.head
    }.sum

  override def partTwo(l: List[String]): Long =
    l.map(extractLongs).collect {
      case is if isPossible(is.head, is.tail, List("*", "+", "||")) => is.head
    }.sum

  private def isPossible(target: Long, numbers: List[Long], possibleOperators: List[String]): Boolean =
    Itertools.cartesianProduct(possibleOperators, numbers.size - 1).exists( operators =>
      interpret(numbers.tail, operators, numbers.head) == target
    )

  private def interpret(numbers: List[Long], operators: List[String], initial: Long): Long =
    numbers.zip(operators).foldLeft(initial){ case (runningTotal, (n, op)) =>
      op match
        case "+" => runningTotal + n
        case "*" => runningTotal * n
        case _ => (runningTotal.toString + n.toString).toLong
    }
}

object DaySevenData extends TestData[Long, Long] {
  override val testData: List[String] = List(
    "190: 10 19",
    "3267: 81 40 27",
    "83: 17 5",
    "156: 15 6",
    "7290: 6 8 6 15",
    "161011: 16 10 13",
    "192: 17 8 14",
    "21037: 9 7 18 13",
    "292: 11 6 16 20"
  )
  override val expectedPartOne: Option[Long] = Some(3749L)
  override val expectedPartTwo: Option[Long] = Some(11387L)
}
