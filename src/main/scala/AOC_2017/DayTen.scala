package AOC_2017

import shared.{DayChallenge, TestData}

object DayTen extends DayChallenge[Int, String]{

  override def partOne(l: List[String]): Int = {
    val storedValues = (0 until getListSize(l)).toList.map(i => i -> i).toMap
    val lengths = l.head.split(",").map(_.toInt).toList
    val result = scramble(lengths, 0, storedValues, 0)._3
    result(0) * result(1)
  }

  private def scramble(lengths: List[Int], startingPosition: Int, startingMap: Map[Int, Int], startingSkipSize: Int)
  : (Int, Int, Map[Int, Int])  =
    lengths.foldLeft((startingPosition, startingSkipSize, startingMap)){ case ((position, skipSize, map), l) =>
      (position + l + skipSize, skipSize + 1, swapWithWraparound(position until position + l, map))
    }

  private def swapWithWraparound(fromToRange: Range, storedValues: Map[Int, Int]): Map[Int, Int] = {
    val indices = fromToRange.toList.map( _ % storedValues.size)
    val valuesAtIndicesReversed = indices.map(storedValues).reverse
    indices.zip(valuesAtIndicesReversed).foldLeft(storedValues){case (map, (i, v)) => map.updated(i, v)}
  }

  def knotHash(s: String): String = {
    val lengths: List[Int] = s.map(_.toByte.toInt).toList ++ List(17, 31, 73, 47, 23)
    val storedValues = (0 to 255).toList.map(i => i -> i).toMap

    val (_, _, endValues) = (0 to 63).foldLeft((0, 0, storedValues)){case ((position, skipSize, storedValues), _) =>
      scramble(lengths, position, storedValues, skipSize)
    }
    combineGroupsOf16WithXor(endValues).map(i => padToTwoDigits(i.toHexString)).mkString
  }

  override def partTwo(l: List[String]): String = knotHash(l.head)

  private def combineGroupsOf16WithXor(numbers: Map[Int, Int]): List[Int] = {
    val range = (0 to 15).toList
    val splitInto16s = range.map(multiple => range.map(remainder => numbers(multiple * 16 + remainder)))
    splitInto16s.map(sixteen => sixteen.tail.fold(sixteen.head)(_ ^ _))
  }

  private def padToTwoDigits(s: String) = if (s.size == 1) s"0$s" else s

  private def getListSize(l: List[String]) = if (l == testData) 5 else 256
}

object DayTenData extends TestData[Int, String] {
  override val expectedPartOne: Option[Int] = Some(12)
  override val testData: List[String] = List("3,4,1,5")
  override val expectedPartTwo: Option[String] = Some("63960835bcdc130f0b66d7ff4f6a5a8e")
  override val testData2: Option[List[String]] = Some(List("1,2,4"))
}
