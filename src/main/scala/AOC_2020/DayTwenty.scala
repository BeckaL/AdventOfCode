package AOC_2020

import shared.{Coord, DayChallenge}

object DayTwenty extends DayChallenge[Long, Int] {
  override def partOne(l: List[String]): Long = {
    val grid = getGrid(l)
    println(grid.map(row => printRow(row.map(_.rows))).mkString("\n\n"))
    grid.head.head.id.toLong * grid.head.last.id.toLong * grid.last.head.id.toLong * grid.last.last.id.toLong
  }

  private def getGrid(l: List[String]): List[List[Tile]] = {
    val tiles = getTiles(l)
    val firstTile = getCorners(tiles).head
    getAllRows(firstTile, tiles, getUnplaced(List(firstTile), tiles))
  }

  private def getAllRows(firstTile: Tile, all: List[Tile], unplaced: List[Tile]) = {
      val firstRow = fillRowFrom(firstTile, getUnplaced(List(firstTile), unplaced))
      val edgeOfLastRow = firstRow.head
    val fillDirection = if (hasSideMatching(Top, edgeOfLastRow, unplaced)) {
      Top
    } else if (hasSideMatching(Bottom, edgeOfLastRow, unplaced)) {
      Bottom
    } else {
      throw new RuntimeException(s"wanted to get a tile to the top or bottom of ${edgeOfLastRow.id} but didn't find any")
    }
    def go(lastPlaced: Tile, fillDirection: Side, currentRows: List[List[Tile]]): List[List[Tile]] =  {
      val unplaced = all.filterNot(t => currentRows.flatten.map(_.id).contains(t.id))
      if (unplaced.isEmpty) {
        currentRows
      } else {
        val candidates = unplaced.filter(t2 => t2 != lastPlaced && hasOneSideMatching(lastPlaced, t2))
        val startOfNextRow = candidates.find(hasSideMatching(fillDirection, lastPlaced, _))
        val startRotatedCorrectly = startOfNextRow match {
          case Some(t) =>
            val rotatedTile = rotateOrFlipUntilMatches(lastPlaced, fillDirection, t)
            (rotatedTile, fillDirection)
          case None => throw new RuntimeException(s"wanted to get a tile to the top or bottom of ${edgeOfLastRow.id} but didn't find any")
        }
        val newRow = fillRowFrom(startRotatedCorrectly._1, getUnplaced(List(startRotatedCorrectly._1), unplaced))

        val newRows: List[List[Tile]] = if (fillDirection == Top) {
          newRow +: currentRows
        } else {
          currentRows :+ newRow
        }
        go(newRow.last, fillDirection, newRows)
      }
    }
    go(edgeOfLastRow, fillDirection, List(firstRow))
  }

  private def getUnplaced(placed: List[Tile], all: List[Tile]) =
    all.filterNot(t => placed.map(_.id).contains(t.id))

  private def fillRowFrom(rowStart: Tile, unplacedTiles: List[Tile]): List[Tile] = {
    val fillDirection = if (hasSideMatching(Left, rowStart, unplacedTiles)) {
      Left
    } else if (hasSideMatching(Right, rowStart, unplacedTiles)) {
      Right
    } else {
      throw new RuntimeException(s"wanted to get a tile to the side of ${rowStart} but didn't find any")
    }
    def go(justPlaced: Tile, unplacedTiles: List[Tile], currentRow: List[Tile]): List[Tile] = {
      val candidates = unplacedTiles.filter(t2 => t2 != justPlaced && hasOneSideMatching(justPlaced, t2))
      val nextTile = candidates.find(hasSideMatching(fillDirection, justPlaced, _))
      nextTile match {
        case Some(t2) =>
          val rotatedTile = rotateOrFlipUntilMatches(justPlaced, fillDirection, t2)
          val newRow = fillDirection match {
            case Left => rotatedTile +: currentRow
            case Right => currentRow :+ rotatedTile
          }
          go(rotatedTile, getUnplaced(List(rotatedTile), unplacedTiles), newRow)
        case None =>  val finalRow = currentRow
          finalRow
      }
    }
    go(rowStart, getUnplaced(List(rowStart), unplacedTiles), List(rowStart))
  }

  private def printRow(tiles: List[List[String]]): String = {
    tiles.head.head.indices.map{i => tiles.map(t => "|" + t(i) + "|").mkString("  ")}.mkString("\n")
  }

  private def getCorners(tiles: List[Tile]): List[Tile] =
    tiles.filter { t1 => tiles.count(t2 => t1 != t2 && hasOneSideMatching(t1, t2)) == 2 }

  private def hasSideMatching(side: Side, t1: Tile, tiles: List[Tile]): Boolean = tiles.exists(t2 => t2 != t1 && hasSideMatching(side, t1, t2))

  private def hasSideMatching(side: Side, t1: Tile, t2: Tile) = t2.sidesAndFlippedSides.contains(side.get(t1))

  private def hasOneSideMatching(t1: Tile, t2: Tile): Boolean =
    t1.sidesAndFlippedSides.exists(side => t2.sidesAndFlippedSides.contains(side))

  private def rotateOrFlipUntilMatches(placedTile: Tile, side: Side, t2: Tile) = {
    if (t2.sidesAndFlippedSides.contains(side.get(placedTile))) {
      rotateUntilMatches(placedTile, side, t2)
    } else {
      throw new RuntimeException(s"Tried to flip tile \n${t2.rows.mkString("\n")} \n\nto match placed tile \n${placedTile.rows.mkString("\n")} on the $side but didn't work")
    }
  }

  private def rotateUntilMatches(placedTile: Tile, side: Side, tileFlippedCorrectly: Tile): Tile = {
    val sideToMatchOnTile2 = side.opposite
    val stringToMatch = side.get(placedTile)
    def go(t2: Tile, i: Int): Tile = {
      if (i > 4) {
        throw new RuntimeException(s"Tried to rotate tile \n${tileFlippedCorrectly.rows.mkString("\n")}\n\n to match \n${placedTile.rows.mkString("\n")}\n\n side $side ${side.get(placedTile)} but didn't work")
      } else {
        if (sideToMatchOnTile2.get(t2) == stringToMatch) {
          t2
        } else if (sideToMatchOnTile2.get(t2).reverse == stringToMatch) {
          side match {
            case Left => t2.flippedHorizontally.rotated90.rotated90
            case Right => t2.flippedHorizontally.rotated90.rotated90
            case Top => t2.flippedVertically.rotated90.rotated90
            case Bottom => t2.flippedVertically.rotated90.rotated90
          }
        } else {
          go(t2.rotated90, i + 1)
        }
      }
    }
    go(tileFlippedCorrectly, 0)
    }

  private def getTiles(l: List[String]): List[Tile] =
    l.mkString("\n").split("\n\n").map { line =>
      val split = line.split("\n")
      val id = split.head.filter(_.isDigit).mkString("").toInt
      Tile(id, split.tail.toList)
    }.toList

  case class Tile(id: Int, rows: List[String]) {
    def asString: String = rows.mkString("\n")
    val withoutBorder = rows.tail.dropRight(1).map(_.tail.dropRight(1))

    val top = rows.head
    val bottom = rows.last
    val left = rows.map(_.head).mkString
    val right = rows.map(_.last).mkString

    def rotated90 = Tile(id, rows.indices.map(i => rows.map(r => r(i)).reverse.mkString).toList)

    def flippedHorizontally = Tile(id, rows.map(_.reverse))
    def flippedVertically = Tile(id, rows.reverse)

    def flippedSides = sides.map(s => s.reverse)

    def sides = List(top, bottom, left, right)

    def sidesAndFlippedSides = sides.flatMap(s => List(s, s.reverse))
  }


  override def partTwo(l: List[String]): Int = {
    val grid = getGrid(l)
    println("String is ")
    val gridAsStringWithoutBorders = grid.map{row =>
      row.head.withoutBorder.indices.map(i => row.map(tile => tile.withoutBorder(i)).mkString("")).mkString("\n")
    }.mkString("\n")
    countAllSeaMonsterVariations(gridAsStringWithoutBorders)
  }

  private def countAllSeaMonsterVariations(image: String): Int = {
    val split = image.split("\n").toList
    val seaMonster = List(
      "                  # ",
      "#    ##    ##    ###",
      " #  #  #  #  #  #   ")
    val rotated90 = rotate(split)
    val rotated180 = rotate(rotated90)
    val rotated270 = rotate(rotated180)
    val flipped = flip(split)
    val flipped90 = rotate(flipped)
    val flipped180 = rotate(flipped90)
    val flipped270 = rotate(flipped180)
    val all = List(split, rotated90, rotated180, rotated270, flipped, flipped90, flipped180, flipped270)
    val allCounts: Map[List[String], List[Coord]] = all.map(image => image -> getCoordsOfSeaMonsters(image, seaMonster)).toMap
    val (correctImage, coords) = allCounts.find{case(image, coords) => coords.size > 0}.get
    val allHashes = correctImage.flatten.count(_ == '#')
    allHashes - (coords.size * seaMonster.flatten.count(_ == '#'))
  }

  def rotate(l: List[String]) =  l.indices.map(i => l.map(r => r(i)).reverse.mkString).toList
  def flip(l: List[String]) = l.reverse

  private def getCoordsOfSeaMonsters(splitImage: List[String], seaMonster: List[String]): List[Coord] = {
    val seaMonsterCoords = getSeaMonsterCoordinates(seaMonster)
    scanGridForSeaMonster(seaMonsterCoords, 0, splitImage, seaMonster.size, seaMonster.head.size, List())
  }

  private def scanRowForSeaMonster(monsterCoords: List[Coord], yStart: Int, grid: List[String], seaMonsterHeight: Int, seaMonsterWidth: Int, x: Int, currentCoords: List[Coord]): List[Coord] =
    if ((x + seaMonsterWidth) > grid.head.indices.end) {
      currentCoords
    } else {
      val slice = grid.slice(yStart, yStart + seaMonsterHeight).map(_.slice(x, x + seaMonsterWidth))
      val monsterFound = getCoords(slice, monsterCoords).forall(_ == '#')
      val newCount = if (monsterFound) {
        currentCoords :+ Coord(x, yStart)
      } else {
        currentCoords
      }
      scanRowForSeaMonster(monsterCoords, yStart, grid, seaMonsterHeight, seaMonsterWidth, x + 1, newCount)
    }

  private def scanGridForSeaMonster(monsterCoords: List[Coord], y: Int, grid: List[String], seaMonsterHeight: Int, seaMonsterWidth: Int, currentCoords: List[Coord]): List[Coord] =
    if ((y + seaMonsterHeight) > grid.indices.end) {
      currentCoords
    } else {
      val newCount = currentCoords ++ scanRowForSeaMonster(monsterCoords, y, grid, seaMonsterHeight, seaMonsterWidth, 0, List())
      scanGridForSeaMonster(monsterCoords, y + 1, grid, seaMonsterHeight, seaMonsterWidth, newCount)
    }

  private def getCoords(strings: List[String], coordsToGet: List[Coord]): List[Char] = coordsToGet.map(c => strings(c.y)(c.x))

  def getSeaMonsterCoordinates(monster: List[String]): List[Coord] =
    monster.zipWithIndex.flatMap{case(row, yIndex) =>
      row.zipWithIndex.collect{case(char, xIndex) if char == '#' => Coord(xIndex, yIndex)}
    }

  trait Side {
    def get(tile: Tile): String
    val opposite: Side
  }

  case object Top extends Side {
    def get(tile: Tile) = tile.top
    override val opposite: Side = Bottom
  }

  case object Bottom extends Side {
    def get(tile: Tile) = tile.bottom
    override val opposite: Side = Top
  }

  case object Left extends Side {
    def get(tile: Tile) = tile.left
    override val opposite: Side = Right
  }

  case object Right extends Side {
    def get(tile: Tile) = tile.right
    override val opposite: Side = Left
  }



  override val expectedPartOne: Option[Long] = Some("20899048083289".toLong)
  override val expectedPartTwo: Option[Int] = Some(273)
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


}
