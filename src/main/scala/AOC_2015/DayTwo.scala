package AOC_2015

import shared.DayChallenge

object DayTwo extends DayChallenge[Long, Long] {
  override def partOne(l: List[String]): Long = getCuboids(l).map(_.wrappingPerimeter).sum

  override def partTwo(l: List[String]): Long = getCuboids(l).map(c => c.ribbonSize).sum

  case class Cuboid(w: Long, h: Long, l: Long) {
    private val s1Area = w * h
    private val s2Area = h * l
    private val s3Area = l * w
    def wrappingPerimeter = (s1Area + s2Area + s3Area) * 2 + List(s1Area, s2Area, s3Area).min

    def ribbonSize = (w * h * l) + List(w, h, l).sorted.take(2).sum * 2
  }

  private def getCuboids(l: List[String]): List[Cuboid] = l.map{str =>
    val split = str.split("x").toList.map(_.toLong)
    Cuboid(split(0), split(1), split(2))
  }


  override val expectedPartOne: Option[Long] = Some(101)
  override val expectedPartTwo: Option[Long] = Some(48)
  override val testData: List[String] = List("2x3x4", "1x1x10")

}
