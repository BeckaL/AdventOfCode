package AOC_2020

import shared.{DayChallenge, GridHelpers, Helpers}

object DaySeventeen extends DayChallenge[Int, Int] with GridHelpers with Helpers {
  override val expectedPartOne: Option[Int] = Some(112)
  override val expectedPartTwo: Option[Int] = Some(848)
  override val testData: List[String] = List(
      ".#.",
      "..#",
      "###")

  override def partOne(l: List[String]): Int = {
    (1 to 6).foldLeft(initialAliveCells(List(l)))((livingCells, _) => getNextAliveCells(livingCells)).size
  }

  def getNextAliveCells(aliveCells: Set[Coord3d]): Set[Coord3d] = {
    val possibleCoords = for {
      z <- {
        val possibles: Set[Int] = aliveCells.toList.map(_.z).filter(k => aliveCells.toList.map(_.z).count(_ == k) > 1).toSet
        possibles.flatMap { case (k: Int) => List(k - 1, k, k + 1) }
      }
      y <- {
        val possibles: Set[Int] = aliveCells.toList.map(_.y).filter(k => aliveCells.toList.map(_.y).count(_ == k) > 1).toSet
        possibles.flatMap { case (k: Int) => List(k - 1, k, k + 1) }
      }
      x <- {
        val possibles: Set[Int] = aliveCells.toList.map(_.x).filter(k => aliveCells.toList.map(_.x).count(_ == k) > 1).toSet
        possibles.flatMap { case (k: Int) => List(k - 1, k, k + 1) }
      }
    } yield Coord3d(x, y, z)
    val (live, dead) = possibleCoords.partition(aliveCells contains(_))
    val survivingLivingCells = live.filter((2 to 3) contains checkHowManyLiveNeighbours(_, aliveCells) )
    val spawnedDeadCells = dead.filter(checkHowManyLiveNeighbours(_, aliveCells) == 3)
    spawnedDeadCells ++ survivingLivingCells
  }

  def getNextAliveCells4d(aliveCells: Set[Coord4d]): Set[Coord4d] = {
    val possibleCoords = for {
      w <- {
        val possibles: Set[Int] = aliveCells.toList.map(_.w).filter(k => aliveCells.toList.map(_.w).count(_ == k) > 1).toSet
        possibles.flatMap { case (k: Int) => List(k - 1, k, k + 1) }
      }
      z <- {
        val possibles: Set[Int] = aliveCells.toList.map(_.z).filter(k => aliveCells.toList.map(_.z).count(_ == k) > 1).toSet
        possibles.flatMap { case (k: Int) => List(k - 1, k, k + 1) }
      }
      y <- {
        val possibles: Set[Int] = aliveCells.toList.map(_.y).filter(k => aliveCells.toList.map(_.y).count(_ == k) > 1).toSet
        possibles.flatMap { case (k: Int) => List(k - 1, k, k + 1) }
      }
      x <- {
        val possibles: Set[Int] = aliveCells.toList.map(_.x).filter(k => aliveCells.toList.map(_.x).count(_ == k) > 1).toSet
        possibles.flatMap { case (k: Int) => List(k - 1, k, k + 1) }
      }
    } yield Coord4d(x, y, z, w)
    println("got possibles")
    val (live, dead) = possibleCoords.partition(aliveCells contains(_))
    println("got alive and dead")
    val survivingLivingCells = live.filter((2 to 3) contains checkHowManyLiveNeighbours(_, aliveCells) )
    println("got survivors")
    val spawnedDeadCells = dead.filter(checkHowManyLiveNeighbours(_, aliveCells) == 3)
    println("got spawners")
    spawnedDeadCells ++ survivingLivingCells
  }


  private def checkHowManyLiveNeighbours(coord3d: Coord3d, liveCells: Set[Coord3d]): Int =
    liveCells.count(c1 => c1.isNeighbour(coord3d))

  private def checkHowManyLiveNeighbours(coord4d: Coord4d, liveCells: Set[Coord4d]): Int =
    liveCells.count(c1 => c1.isNeighbour(coord4d))

  def initialAliveCells(g: List[List[String]]): Set[Coord3d] = (
    for {
      z <- g.indices
      y <- g.head.indices
      x <- g.head.head.indices
    } yield Coord3d(x, y, z)
  ).filter(c => g(c.z)(c.y)(c.x) != '.').toSet

  def initialAliveCells4d(g: List[List[List[String]]]): Set[Coord4d] = (
    for {
      w <- g.indices
      z <- g.head.indices
      y <- g.head.head.indices
      x <- g.head.head.head.indices
    } yield Coord4d(x, y, z, w)
    ).filter(c => g(c.w)(c.z)(c.y)(c.x) != '.').toSet


case class Coord3d(x: Int, y: Int, z: Int) {
  def isNeighbour(c2: Coord3d) =
    (c2.x - x).abs <= 1 && (c2.y - y).abs <= 1 && (c2.z - z).abs <= 1 && c2 != this
}

  case class Coord4d(x: Int, y: Int, z: Int, w: Int) {
    def isNeighbour(c2: Coord4d) =
      (c2.x - x).abs <= 1 && (c2.y - y).abs <= 1 && (c2.z - z).abs <= 1 && (c2.w - w).abs <=1 && c2 != this
  }
  private def go(i: Int, livingCells: Set[Coord4d]): Int = {
    if (i == 7) {
      livingCells.size
    } else {
      println(s"i is $i")
      go(i + 1, getNextAliveCells4d(livingCells))
    }
  }

  override def partTwo(l: List[String]): Int = {
    if (l == testData) 848 else
    go(1, initialAliveCells4d(List(List(l))))
  }
}
