package AOC_2022

import shared.{DayChallenge, Helpers, TestData, Coord}

object DayFifteen extends DayChallenge[Int, Long] with Helpers {
  override def partOne(l: List[String]): Int = {
    val row = if (l == DayFifteenData.testData) 10 else 2000000
    val sensorsAndRanges = sensors(l)
    val beaconsInRow = sensorsAndRanges.map(_.beacon).count(_.y == row)
    sensorsAndRanges.flatMap { s => sensorRangeInRow(row, s).toSet }.size - beaconsInRow
  }

  override def partTwo(l: List[String]): Long = {
    val max = if (l == DayFifteenData.testData) 20 else 4000000
    val c = findCoordNotCoveredBySensor(sensors(l), Coord(0, 0), max)
    c.x * 4000000L + c.y
  }

  private def sensorRangeInRow(row: Int, sensor: Sensor): Range = {
    val eitherSide = (sensor.range - (row - sensor.pos.y).abs)
    sensor.pos.x - eitherSide to sensor.pos.x + eitherSide
  }

  private def findCoordNotCoveredBySensor(sensors: Set[Sensor], c: Coord, max: Int): Coord =
    sensors.find { sensor => manhattanDistance(c, sensor.pos) <= sensor.range } match {
      case None => c
      case Some(sensor) =>
        val nextUnknownX = sensorRangeInRow(c.y, sensor).last + 1
        val nextCoord = if (nextUnknownX > max) Coord(0, c.y + 1) else Coord(nextUnknownX, c.y)
        findCoordNotCoveredBySensor(sensors, nextCoord, max)
    }

  private def manhattanDistance(a: Coord, b: Coord): Int = (a.x - b.x).abs + (a.y - b.y).abs

  private def sensors(l: List[String]): Set[Sensor] =
    l.map(extractIntsWithOptionalSigns)
      .map(ints => Sensor(Coord(ints(0), ints(1)), Coord(ints(2), ints(3))))
      .toSet

  case class Sensor(pos: Coord, beacon: Coord) {
    val range = manhattanDistance(pos, beacon)
  }
}

object DayFifteenData extends TestData[Int, Long] {
  override val testData: List[String] = List(
    "Sensor at x=2, y=18: closest beacon is at x=-2, y=15",
    "Sensor at x=9, y=16: closest beacon is at x=10, y=16",
    "Sensor at x=13, y=2: closest beacon is at x=15, y=3",
    "Sensor at x=12, y=14: closest beacon is at x=10, y=16",
    "Sensor at x=10, y=20: closest beacon is at x=10, y=16",
    "Sensor at x=14, y=17: closest beacon is at x=10, y=16",
    "Sensor at x=8, y=7: closest beacon is at x=2, y=10",
    "Sensor at x=2, y=0: closest beacon is at x=2, y=10",
    "Sensor at x=0, y=11: closest beacon is at x=2, y=10",
    "Sensor at x=20, y=14: closest beacon is at x=25, y=17",
    "Sensor at x=17, y=20: closest beacon is at x=21, y=22",
    "Sensor at x=16, y=7: closest beacon is at x=15, y=3",
    "Sensor at x=14, y=3: closest beacon is at x=15, y=3",
    "Sensor at x=20, y=1: closest beacon is at x=15, y=3"
  )
  override val expectedPartOne: Option[Int] = Some(26)
  override val expectedPartTwo: Option[Long] = Some(56000011)
}