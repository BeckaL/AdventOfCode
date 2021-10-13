package AOC_2017

import shared.{DayChallenge, Helpers, TestData}

object DayTwentyOne extends DayChallenge[Int, Int] with Helpers {
  override def partOne(l: List[String]): Int = {
    val rules = l.map(Rule.from)

    val iterations = if (l == DayTwentyOneData.testData) 2 else 5
    val endTile = translate(rules, iterations)
    endTile.map(_.count(_ == '#')).sum
  }

  override def partTwo(l: List[String]): Int = {
    val rules = l.map(Rule.from)
    val iterations = if (l == DayTwentyOneData.testData) 2 else 18
    val endTile = translate(rules, iterations)
    endTile.map(_.count(_ == '#')).sum
  }

  def translate(rules: List[Rule], iterations: Int) = {
    def go(i: Int, currentTile: List[String]): List[String] =
      if (i == iterations) currentTile else {
        val splitIntoSmallerTiles = currentTile.split
        val resultTilesAfterTranslation = splitIntoSmallerTiles.map{ tileRow =>
          tileRow.map(tile => rules.find(_.matchesFrom(tile)).get.to)
        }
        val bigResultTile = joinTiles(resultTilesAfterTranslation)
        go(i + 1, bigResultTile)
      }
    val startTile =   List(
      ".#.",
      "..#",
      "###"
    )
    go(0, startTile)
  }

  def joinTiles(tileRows: List[List[List[String]]]): List[String] = tileRows.flatMap(tileRow => tileRow.head.indices.toList.map(i => tileRow.map(tile => tile(i)).mkString("")))

  case class Rule(from: List[String], to: List[String]) {
    val rotated90 = from.rotate90
    val rotated180 = rotated90.rotate90
    val rotated270 = rotated180.rotate90
    val all = List(from, rotated90, rotated180, rotated270)

    val allIncludingFlipped = all.flatMap(t => List(t, t.flipV, t.flipH))

    def matchesFrom(tile: List[String]) = allIncludingFlipped.contains(tile)
  }



  implicit class TileOps(tile: List[String]) {
    val indices = tile.indices.toList

    def rotate90: List[String] =
      indices.reverse.map(yIndex => indices.map(xIndex => tile(xIndex)(yIndex)).mkString)

    def flipV: List[String] = tile.reverse

    def flipH: List[String] = tile.map(_.reverse)

    def split: List[List[List[String]]] =
      if (tile.size % 2 == 0) splitIntoTwos else splitIntoThrees

    private def splitIntoTwos: List[List[List[String]]] =
      tile.grouped(2).toList.map{
        twoRows =>
        val first = twoRows.head
          val second = twoRows(1)
          first.grouped(2).zip(second.grouped(2)).map{case(firstRow, secondRow) => List(firstRow, secondRow)}.toList
      }

    private def splitIntoThrees: List[List[List[String]]] =
      tile.grouped(3).toList.map{
        threeRows =>
          val first = threeRows.head
          val second = threeRows(1)
          val third = threeRows(2)
          first.grouped(3).zip(second.grouped(3).zip(third.grouped(3))).map{case(firstRow, (secondRow, thirdRow)) => List(firstRow, secondRow, thirdRow)}.toList
      }
  }

  object Rule {
    def from(str: String): Rule = {
      val (fromString, toString) = getTwoFromSplit(str, " => ")
      Rule(fromString.split("/").toList, toString.split("/").toList)
    }
  }

}

object DayTwentyOneData extends TestData[Int, Int] {
  override val testData: List[String] = List(
  "../.# => ##./#../...",
  ".#./..#/### => #..#/..../..../#..#"
  )
  override val expectedPartOne: Option[Int] = Some(12)
  override val expectedPartTwo: Option[Int] = Some(12)
}