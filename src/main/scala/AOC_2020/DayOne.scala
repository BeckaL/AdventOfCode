package AOC_2020

import shared.DayChallenge

object DayOne extends DayChallenge[Int, Int] {
  override val testData: List[String] = List(1721, 979, 366, 299, 675, 1456).map(_.toString)

  override val expectedPartOne: Option[Int] = Some(514579)

  override val expectedPartTwo: Option[Int] = Some(241861950)

  override def partOne(input: List[String]): Int =
    find2NumbersThatSumToN(input.head.toInt, input.tail.map(_.toInt), 2020).product

  override def partTwo(input: List[String]): Int = findThreeNumbersThatSumTo2020(input.map(_.toInt).sorted).product

  private def findThreeNumbersThatSumTo2020(orderedL: List[Int]): (Int, Int, Int) = {
    val possibleSearchSpace = orderedL.takeWhile(_ + orderedL.take(2).sum <= 2020)
    val firstN = possibleSearchSpace.head
    find2NumbersThatSumToN(possibleSearchSpace(1), possibleSearchSpace.drop(2), 2020 - firstN) match {
      case Some((i, k)) => (firstN, i, k)
      case None => findThreeNumbersThatSumTo2020(possibleSearchSpace.tail)
    }
  }

  private def find2NumbersThatSumToN(i: Int, l: List[Int], n: Int): Option[(Int, Int)] = l match {
    case _ :: _ =>
      l.find(i + _ == n) match {
        case Some(k) => Some(i, k)
        case None => find2NumbersThatSumToN(l.head, l.tail, n)
      }
    case _ => None
  }

  implicit class OptionOfTupleOps(opt: Option[(Int, Int)]) {
    def product: Int = opt.get._1 * opt.get._2
  }

  implicit class Tuple3Ops(tup: (Int, Int, Int)) {
    def product: Int = tup._1 * tup._2 * tup._3
  }

}
