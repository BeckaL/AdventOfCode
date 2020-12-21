package AOC_2020

import shared.DayChallenge

object DayTwenty extends  DayChallenge[Long, Long]{
  override val expectedPartOne: Option[Long] = Some(0)
  override val expectedPartTwo: Option[Long] = Some(0)
  override val testData: List[String] = List(
      "Tile 2311:",
      "..##.#..#.",
      "##..#.....",
      "#...##..#.",
      "####.#...#",
      "##.##.###.",
      "##...#.###",
      ".#.#.#..##",
      "..#....#..",
      "###...#.#.",
      "..###..###",
      "",
      "Tile 1951:",
      "#.##...##.",
      "#.####...#",
      ".....#..##",
      "#...######",
      ".##.#....#",
      ".###.#####",
      "###.##.##.",
      ".###....#.",
      "..#.#..#.#",
      "#...##.#..",
      "",
      "Tile 1171:",
      "####...##.",
      "#..##.#..#",
      "##.#..#.#.",
      ".###.####.",
      "..###.####",
      ".##....##.",
      ".#...####.",
      "#.##.####.",
      "####..#...",
      ".....##...",
      "",
      "Tile 1427:",
      "###.##.#..",
      ".#..#.##..",
      ".#.##.#..#",
      "#.#.#.##.#",
      "....#...##",
      "...##..##.",
      "...#.#####",
      ".#.####.#.",
      "..#..###.#",
      "..##.#..#.",
      "",
      "Tile 1489:",
      "##.#.#....",
      "..##...#..",
      ".##..##...",
      "..#...#...",
      "#####...#.",
      "#..#.#.#.#",
      "...#.#.#..",
      "##.#...##.",
      "..##.##.##",
      "###.##.#..",
      "",
      "Tile 2473:",
      "#....####.",
      "#..#.##...",
      "#.##..#...",
      "######.#.#",
      ".#...#.#.#",
      ".#########",
      ".###.#..#.",
      "########.#",
      "##...##.#.",
      "..###.#.#.",
      "",
      "Tile 2971:",
      "..#.#....#",
      "#...###...",
      "#.#.###...",
      "##.##..#..",
      ".#####..##",
      ".#..####.#",
      "#..#.#..#.",
      "..####.###",
      "..#.#.###.",
      "...#.#.#.#",
      "",
      "Tile 2729:",
      "...#.#.#.#",
      "####.#....",
      "..#.#.....",
      "....#..#.#",
      ".##..##.#.",
      ".#.####...",
      "####.#.#..",
      "##.####...",
      "##..#.##..",
      "#.##...##.",
      "",
      "Tile 3079:",
      "#.#.#####.",
      ".#..######",
      "..#.......",
      "######....",
      "####.#..#.",
      ".#...#.##.",
      "#.#####.##",
      "..#.###...",
      "..#.......",
      "..#.###...")
  override def partOne(l: List[String]): Long = {
    val tileStrings = l.mkString("\n").split("\n\n").map(_.split("\n"))
    val ts = tileStrings.map(t => Tile(t.head.filter(_.isDigit).mkString.toInt, t.tail.toList))
    val matches = ts.map{t => findNumberOfMatches(t, ts.toList)}
    println(matches.sorted.toList)
    0
  }

  private def findNumberOfMatches(t: Tile, all: List[Tile]): Int =
    all.count{ t2 => t2.allSides.intersect(t.allSides).size > 1}

  override def partTwo(l: List[String]): Long = ???
  private def sidesOfTile(rows: List[String]) =
    List(rows.head, rows.tail, rows.map(_ (0)), rows.map(_ (rows.size - 1)))

  case class Tile(id: Int, rows: List[String]) {
    val rotated90 = rows.indices.map(yIndex => rows.indices.map(xIndex => rows(rows.size - 1 - xIndex)(yIndex)).mkString).toList
    val rotated180 = rows.reverse.map(row => row.reverse)
    val rotated270 = rows.indices.map(i => rows.map(_(rows.size - 1 - i)).mkString).toList
    val allRotated = List(rows, rotated90, rotated180, rotated270)
    val flipped = allRotated.map(_.reverse)
    val allSides = (allRotated ++ flipped).map(sidesOfTile)

  }
}
