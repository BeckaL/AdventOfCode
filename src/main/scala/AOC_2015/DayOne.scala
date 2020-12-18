package AOC_2015

import shared.DayChallenge

object DayOne extends DayChallenge[Int, Int] {
  override val expectedPartOne: Option[Int] = Some(-3)
  override val expectedPartTwo: Option[Int] = Some(5)
  override val testData: List[String] = List(")())())")
  override val testData2: Option[List[String]] = Some(List("()())"))
  override def partOne(l: List[String]): Int = l.head.count(_ == '(') * 1 + l.head.count(_ == ')') * -1

  override def partTwo(l: List[String]): Int = {
    def go(str: String, i: Int, floor: Int): Int = {
      val newFloor = if (str.head == '(') floor + 1 else floor - 1
      if (newFloor == -1) i + 1 else go(str.tail, i + 1, newFloor)
      }
    go(l.head, 0, 0)
  }
}
