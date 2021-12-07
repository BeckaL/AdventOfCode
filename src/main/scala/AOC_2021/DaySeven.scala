package AOC_2021

import shared.{DayChallenge, TestData}

object DaySeven extends DayChallenge[Int, Int] {
  override def partOne(l: List[String]): Int = {
    val ns      = l.head.split(",").map(_.toInt).toList
    val average = ns.sum / ns.size
    val startNo = ns.map(findFuelRequired(average, _)).sum
    find(average, 1, startNo, ns, average, findFuelRequired)
  }

  // there's a nicer way of doing this but time
  def find(start: Int, iteration: Int, currentSmallest: Int, is: List[Int], maxIterations: Int, f: (Int, Int) => Int): Int = {
    if (iteration > maxIterations) {
      currentSmallest
    } else {
      val i1 = start + iteration
      val i2 = start - iteration

      val i1Number = is.map(f(_, i1)).sum
      val i2Number = is.map(f(_, i2)).sum

      val newCurrentSmallest = Seq(currentSmallest, i1Number, i2Number).min

      find(start, iteration + 1, newCurrentSmallest, is, maxIterations, f)
    }
  }

  def findFuelRequired(from: Int, to: Int): Int = (to - from).abs

  def findFuelRequiredTriangular(from: Int, to: Int): Int = triangularNumber(findFuelRequired(from, to))

  private def triangularNumber(n: Int) = (n * (n + 1)) / 2

  override def partTwo(l: List[String]): Int = {
    val ns      = l.head.split(",").map(_.toInt).toList
    val average = ns.sum / ns.size
    val startNo = ns.map(findFuelRequiredTriangular(average, _)).sum
    find(average, 1, startNo, ns, average, findFuelRequiredTriangular)
  }
}

object DaySevenData extends TestData[Int, Int] {
  override val testData: List[String]       = List("16,1,2,0,4,2,7,1,2,14")
  override val expectedPartOne: Option[Int] = Some(37)
  override val expectedPartTwo: Option[Int] = Some(168)
}
