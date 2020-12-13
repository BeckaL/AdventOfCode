package AOC_2020

import shared.DayChallenge

object DayThirteen extends DayChallenge[Int, Long] {
  override def partOne(l: List[String]): Int = {
    val earliestTimestamp = l.head.toInt
    val (minBus, minTime) = getBuses(l)
      .map { b => b -> findClosestTo(earliestTimestamp, b.route).toInt }
      .minBy(_._2)
    (minTime - earliestTimestamp) * minBus.route.toInt
  }

  override def partTwo(l: List[String]): Long =
    getBuses(l).reduce((b1, b2) => getCombinedBus(b1,b2)).timestamp

  private def getBuses(l: List[String]): List[Bus] =
    l(1).split(",").zipWithIndex.collect { case (busNo, i) if busNo != "x" => Bus(busNo.toInt, i) }.toList

  private def getCombinedBus(bus1: Bus, bus2: Bus): Bus = {
    val newRoute = bus1.route * bus2.route
    def go(i: Long, step: Long): Bus = if (bus2.worksWith(i))
      Bus(newRoute, newRoute - i)
    else
      go(i + step, step)
    go(bus1.route - bus1.remainder, bus1.route)
  }

  case class Bus(route: Long, remainder: Long) {
    def worksWith(i: Long): Boolean = (i + remainder) % route == 0
    def timestamp: Long =  route - remainder
  }

  private def findClosestTo(i: Long, busNo: Long): Long = ((i.toFloat / busNo).ceil * busNo).toLong

  override val testData: List[String] = List(
    "939",
    "7,13,x,x,59,x,31,19"
  )
  override val expectedPartOne: Option[Int] = Some(295)
  override val expectedPartTwo: Option[Long] = Some(1068781)
}
