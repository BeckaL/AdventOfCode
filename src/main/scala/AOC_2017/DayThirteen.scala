package AOC_2017

import shared.{DayChallenge, Helpers, TestData}

object DayThirteen extends DayChallenge[Int, Int] with Helpers {
  override def partOne(l: List[String]): Int =
    l.map(Layer.from).filter(hasClashWithDelay(_, 0)).map(_.severity).sum

  override def partTwo(l: List[String]): Int = {
    val layers = l.map(Layer.from)
    def go(currentDelay: Int): Int = {
      if (!layers.exists(l => hasClashWithDelay(l, currentDelay))) {
        currentDelay
      } else {
        go(currentDelay + 1)
      }
    }
    go(1)
  }

  private def hasClashWithDelay(layer: Layer, delay: Int) = (layer.depth + delay) % (2 * layer.range - 2) == 0

  case class Layer(depth: Int, range: Int) {
    def severity: Int = depth * range
  }

  object Layer {
    def from(s: String): Layer = {
      val (depthString, rangeString) = getTwoFromSplit(s, ": ")
      Layer(depthString.toInt, rangeString.toInt)
    }
  }
}

object DayThirteenData extends TestData[Int, Int] {
  override val expectedPartOne: Option[Int] = Some(24)
  override val expectedPartTwo: Option[Int] = Some(10)
  override val testData: List[String] = "0: 3\n1: 2\n4: 4\n6: 4".split("\n").toList
}
