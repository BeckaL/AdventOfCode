package AOC_2018

import shared.{DayChallenge, Helpers}

object DayTwentyThree extends DayChallenge[Int, Int] with Helpers {
  override val expectedPartOne: Option[Int] = Some(36)
  override val expectedPartTwo: Option[Int] = Some(100000)
  override val testData: List[String] = List(
      "pos=<0,0,0>, r=4",
      "pos=<1,0,0>, r=1",
      "pos=<4,0,0>, r=3",
      "pos=<0,2,0>, r=1",
      "pos=<0,5,0>, r=3",
      "pos=<0,0,3>, r=1",
      "pos=<1,1,1>, r=1",
      "pos=<1,1,2>, r=1",
      "pos=<1,3,1>, r=1"
  )

  override def partOne(l: List[String]): Int = {
    val robots = getRobots(l)
    val biggest = robots.maxBy(_.radius)
    robots.count(r => taxiCabDifferece(biggest.position, r.position) <= biggest.radius)
  }

  private def getRobots(l: List[String]) = l.map{
    str => val (pos, r) = getTwoFromSplit(str, ", ")
      val radius = r.filter(_.isDigit).mkString.toLong
      val (_, positions) = getTwoFromSplit(pos, "=<")
      Robot(radius, positions.replace(">", "").split(",").toList match {
        case List(i1, i2, i3) => Position(i1.toLong, i2.toLong, i3.toLong)
        case _ => throw new RuntimeException("boom")
      })
  }

  def taxiCabDifferece(position1: Position, position2: Position): Long =
    (position1.x - position2.x).abs + (position1.y - position2.y).abs + (position1.z - position2.z).abs

  def taxiCabDifferenceFrom0(position: Position) = position.x.abs + position.y.abs + position.z.abs

  case class Position(x: Long, y: Long, z: Long)
  case class Robot(radius: Long, position: Position)

  override def partTwo(l: List[String]): Int = ???
}
