package AOC_2021

import shared.{Coord, DayChallenge, Helpers, TestData}

import scala.annotation.tailrec

object DaySeventeen extends DayChallenge[Int, Int] with Helpers {
  override def partOne(l: List[String]): Int = {
    val (xRange, yRange) = parse(l.head)
    findHighestY(xRange, yRange)
  }

  override def partTwo(l: List[String]): Int = {
    val (xRange, yRange) = parse(l.head)
//    val possibles = (0 until xRange.max)

//    fire(23, -)

      val possibles = List(23).flatMap(xV=> getPossibleYVelocities(xV, xRange, yRange).map(yV => (xV, yV)))
    println(possibles)
    possibles.size
  }

  private def getPossibleYVelocities(xV: Int, targetX: Range, targetY: Range): List[Int] = {
    def go(yV: Int, found: List[Int]): List[Int] = {
      fire(xV, yV, targetX, targetY) match {
        case Hit =>
          println(s"hit ${xV} ${yV}")
          go(yV + 1, yV +: found)
        case Overshoot if yV > 0 =>
          println(s"overshoot and y is over 0 ${xV} ${yV} ")
          found
        case Miss =>
          println(s"miss ${xV} ${yV}")
          found
        case _ =>
          println(s"overshoot ${xV} ${yV}")
          go(yV + 1, found)
      }
    }
    val yMin = targetY.min
    go(yMin, List())
  }

  private def findHighestY(xRange: Range, yRange: Range): Int =
    (1 to xRange.max / 2 + 1).filter(canHit(_, xRange)).map(xV =>
      findOptimalYandHighestY(xV, xRange,yRange, 1, 0)
    ).max

  private def canHit(x:Int, xRange: Range) = xRange.contains((x * (x + 1)) / 2)

  def findOptimalYandHighestY(xV: Int, xRange: Range, yRange: Range, y: Int, maxFoundSoFar: Int): Int =
    fireGettingMaybeHighestY(xV, y, xRange, yRange) match {
      case Overshoots => maxFoundSoFar
      case MaybeHighest(Some(max)) if max > maxFoundSoFar => findOptimalYandHighestY(xV, xRange, yRange, y + 1, max)
      case _ => findOptimalYandHighestY(xV, xRange, yRange, y + 1, maxFoundSoFar)
    }

  private def parse(s: String): (Range, Range) = {
    val ranges = extractIntsWithOptionalSigns(s).sliding(2, 2)
      .map(rangeInts => Range.inclusive(rangeInts(0), rangeInts(1))).toList
    (ranges(0), ranges(1))
  }

  trait ResultPartOne
  case object Overshoots extends ResultPartOne
  case class MaybeHighest(v: Option[Int]) extends ResultPartOne

  private def fireGettingMaybeHighestY(xV: Int, yV: Int, targetX: Range, targetY: Range): ResultPartOne = {
    @tailrec
    def go(c: Coord, xV: Int, yV: Int, highestY: Int, yVMax: Int): ResultPartOne = {
      if (yV > yVMax) {
        Overshoots
      } else if (yV < 0 && c.y < targetY.min) {
        MaybeHighest(None)
      } else if (c.x > targetX.max && c.y > targetY.max) {
        Overshoots
      } else if (targetX.contains(c.x) && targetY.contains(c.y)) {
        MaybeHighest(Some(highestY))
      } else {
        go(Coord(c.x + xV, c.y + yV), moveTowardsZero(xV), yV - 1, List(c.y, highestY).max, yVMax)
      }
    }
    val yMaxV = targetY.min.abs + 1
    go(Coord(0, 0), xV, yV, 0, yMaxV)
  }

  trait Result
  case object Miss extends Result
  case object Hit extends Result
  case object Overshoot extends Result

  private def fire(xV: Int, yV: Int, targetX: Range, targetY: Range):  Result = {
    @tailrec
    def go(c: Coord, xV: Int, yV: Int, yVMax: Int): Result = {
      if (yV > yVMax) {
        Overshoot
      } else if (yV < 0 && c.y < targetY.min) {
        Miss
      } else if (c.x > targetX.max && c.y > targetY.max) {
        Overshoot
      } else if (targetX.contains(c.x) && targetY.contains(c.y)) {
        Hit
      } else {
        go(Coord(c.x + xV, c.y + yV), moveTowardsZero(xV), yV - 1, yVMax)
      }
    }
    go(Coord(0, 0), xV, yV, targetY.min.abs + 1)
  }

  private def moveTowardsZero(i: Int): Int =
    i match {
      case neg if neg < 0 => i + 1
      case pos if pos > 0 => i - 1
      case _ => 0
    }


}

object DaySeventeenData extends TestData[Int, Int] {
  override val testData: List[String] = List("target area: x=20..30, y=-10..-5")
  override val expectedPartOne: Option[Int] = Some(45)
  override val expectedPartTwo: Option[Int] = Some(112)
}
