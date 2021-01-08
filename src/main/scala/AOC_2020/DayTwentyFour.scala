package AOC_2020

import shared.DayChallenge

import scala.annotation.tailrec

object DayTwentyFour extends DayChallenge[Int, Int] {

  override def partOne(l: List[String]): Int =
   getEndpointsWhichAreBlack(getEndpoints(l)).size

  override def partTwo(l: List[String]): Int = {
    val startingBlackTiles: Set[(Int, Int)] = getEndpointsWhichAreBlack(getEndpoints(l))
    (1 to 100).foldLeft(startingBlackTiles)((blackTiles,_) => nextTick(blackTiles)).size
  }

  private def getEndpointsWhichAreBlack(endpoints: List[(Int, Int)]): Set[(Int, Int)] =
    endpoints.toSet.filter(e => endpoints.count(_ == e) % 2 == 1)

  private def getEndpoints(l: List[String]): List[(Int, Int)] =
    l.map(getDelimitedDirections(_, List())).map(directions => getEndpoint(directions, (0, 0)))

  private def nextTick(blackTiles: Set[(Int, Int)]) = {
    val turningBlack = checkTilesTurningOrRemainingBlack(getWhiteNeighbouringTiles(blackTiles), blackTiles, _ == 2)
    val remainingBlack = checkTilesTurningOrRemainingBlack(blackTiles, blackTiles, (1 to 2).contains(_))
    remainingBlack ++ turningBlack
  }

  private def checkTilesTurningOrRemainingBlack(tilesToIterate: Set[(Int, Int)], blackTiles: Set[(Int, Int)], pred: Int => Boolean) =
    tilesToIterate.filter(t => pred(t.neighbours.count(blackTiles.contains)))

  implicit class HexCoordOps(c: (Int, Int)) {

    def neighbours = {
      val x = c._1
      val y = c._2
      Set((x - 2, y), (x + 2, y), (x - 1, y - 1), (x - 1, y + 1), (x + 1, y - 1), (x + 1, y + 1))
    }
  }

  private def getWhiteNeighbouringTiles(blackTiles: Set[(Int, Int)]) =
    blackTiles.flatMap(_.neighbours).diff(blackTiles)

  private def getEndpoint(directions: List[String], current: (Int, Int)): (Int, Int) = {
    directions.foldLeft(current){case (coord, direction) => direction match {
      case "e" => (coord._1 + 2, coord._2)
      case "w" => (coord._1 - 2, coord._2)
      case "ne" => (coord._1 + 1, coord._2 + 1)
      case "nw" => (coord._1 - 1, coord._2 + 1)
      case "se" =>(coord._1 + 1, coord._2 - 1)
      case "sw" => (coord._1 - 1, coord._2 - 1)
    }
    }
  }

  @tailrec
  private def getDelimitedDirections(s: String, current: List[String] = List()): List[String] = {
    if (s.isEmpty) {
      current
    } else {
      s.take(2) match {
        case e if e.head == 'e' => getDelimitedDirections(s.tail, current :+ "e")
        case w if w.head == 'w' =>getDelimitedDirections(s.tail, current :+ "w")
        case "ne" => getDelimitedDirections(s.drop(2), current :+ "ne")
        case "nw" => getDelimitedDirections(s.drop(2), current :+ "nw")
        case "se" => getDelimitedDirections(s.drop(2), current :+ "se")
        case "sw" => getDelimitedDirections(s.drop(2), current :+ "sw")
      }
    }
  }
  override val expectedPartOne: Option[Int] = Some(10)
  override val expectedPartTwo: Option[Int] = Some(2208)
  override val testData: List[String] = List(
    "sesenwnenenewseeswwswswwnenewsewsw",
    "neeenesenwnwwswnenewnwwsewnenwseswesw",
    "seswneswswsenwwnwse",
    "nwnwneseeswswnenewneswwnewseswneseene",
    "swweswneswnenwsewnwneneseenw",
    "eesenwseswswnenwswnwnwsewwnwsene",
    "sewnenenenesenwsewnenwwwse",
    "wenwwweseeeweswwwnwwe",
    "wsweesenenewnwwnwsenewsenwwsesesenwne",
    "neeswseenwwswnwswswnw",
    "nenwswwsewswnenenewsenwsenwnesesenew",
    "enewnwewneswsewnwswenweswnenwsenwsw",
    "sweneswneswneneenwnewenewwneswswnese",
    "swwesenesewenwneswnwwneseswwne",
    "enesenwswwswneneswsenwnewswseenwsese",
    "wnwnesenesenenwwnenwsewesewsesesew",
    "nenewswnwewswnenesenwnesewesw",
    "eneswnwswnwsenenwnwnwwseeswneewsenese",
    "neswnwewnwnwseenwseesewsenwsweewe",
    "wseweeenwnesenwwwswnew")

}
