package AOC_2022

import shared.{DayChallenge, TestData, Helpers}

object DayFour extends DayChallenge[Int, Int] with Helpers{
  override def partOne(l: List[String]): Int =
    l.map(line => parse(line)).count{
      case (a, b) => a._1 <= b._1 && a._2 >= b._2 || b._1 <= a._1 && b._2 >= a._2
    }

  override def partTwo(l: List[String]): Int =
    l.map(line => parse(line)).count{
      case (a, b) => a._1 <= b._1 && a._2 >= b._1 || a._1 <= b._2 && a._1 >= b._1
    }

  private def parse(line: String): ((Int, Int), (Int, Int)) = {
    val pairs = line.split(",").map{range =>
      val (a, b) = getTwoFromSplit(range, "-")
      (a.toInt, b.toInt)
    }
    (pairs(0), pairs(1))
  }
}

object DayFourData extends TestData[Int, Int] {
  //didn't need these today
  override val testData: List[String] = List()
  override val expectedPartOne: Option[Int] = None
  override val expectedPartTwo: Option[Int] = None
}