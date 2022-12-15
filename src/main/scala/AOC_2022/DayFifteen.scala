package AOC_2022

import shared.{DayChallenge, Helpers, TestData, Coord}

object DayFifteen extends DayChallenge[Int, Long] with Helpers {
  override def partOne(l: List[String]): Int = {
    val row = if (l == DayFifteenData.testData) 10 else 2000000
    val sensorsAndRanges = sensors(l)
    sensorsAndRanges
      .flatMap { s => sensorRangeInRow(row, s).toSet }
      .diff(sensorsAndRanges.map(_.beacon).filter(_.y == row).map(_.x))
      .size
  }

  override def partTwo(l: List[String]): Long = {
    val max = if (l == DayFifteenData.testData) 20 else 4000000
    val c = findCoordNotCoveredBySensor(sensors(l), 0, 0, max)
    c.x * 4000000L + c.y
  }

  private def sensorRangeInRow(row: Int, sensor: Sensor): Range = {
    val eitherSide = (sensor.range - (row - sensor.pos.y).abs)
    sensor.pos.x - eitherSide to sensor.pos.x + eitherSide
  }

  private def findCoordNotCoveredBySensor(sensorsAndRanges: Set[Sensor], row: Int, col: Int, max: Int): Coord =
    if (col > max)
      findCoordNotCoveredBySensor(sensorsAndRanges, row + 1, 0, max)
    else
      sensorsAndRanges
        .find { sensor => manhattanDistance(Coord(col, row), sensor.pos) <= sensor.range
        } match {
        case None => Coord(col, row)
        case Some(sensor) =>
          val nextXNotCoveredBySensor = sensorRangeInRow(row, sensor).last + 1
          findCoordNotCoveredBySensor(sensorsAndRanges, row, nextXNotCoveredBySensor, max)
      }

  private def manhattanDistance(a: Coord, b: Coord): Int = (a.x - b.x).abs + (a.y - b.y).abs

  private def sensors(l: List[String]): Set[Sensor] =
    l.map(extractIntsWithOptionalSigns)
      .map { ints =>
        val sensor = Coord(ints(0), ints(1))
        val beacon = Coord(ints(2), ints(3))
        Sensor(sensor, beacon)
      }.toSet

  case class Sensor(pos: Coord, beacon: Coord) { val range = manhattanDistance(pos, beacon)}
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