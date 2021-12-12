package AOC_2021

import shared.{Coord, DayChallenge, GridHelpers, TestData}

import scala.annotation.tailrec

object DayEleven extends DayChallenge[Int, Int] with GridHelpers {
  override def partOne(l: List[String]): Int = {
    val withIndices = getOctopiWithIndices(l)
    val (_, finalNumberOfFlashes) = (0 until 100).foldLeft((withIndices, 0)) { case ((octs, currentNumberOfFlashes), _) =>
      val (newOctopi, numberOfFlashes) = tick(octs)
      (newOctopi, currentNumberOfFlashes + numberOfFlashes)
    }
    finalNumberOfFlashes
  }

  def getOctopiWithIndices(l: List[String]): Map[Coord, Int] =
    l.map(_.split("").map(_.toInt).toList).zipWithIndex.flatMap { case (row, yIndex) =>
      row.zipWithIndex.map { case (cell, xIndex) => Coord(xIndex, yIndex) -> cell }
    }.toMap

  def tick(m: Map[Coord, Int]): (Map[Coord, Int], Int) = {
    val afterIncreasingEnergyLevels      = increaseEnergyLevels(m)
    val (afterFlashing, numberOfFlashes) = flashyFlashyRecurse(afterIncreasingEnergyLevels)
    (afterFlashing.map { case (coord, value) => if (value > 9) coord -> 0 else coord -> value }, numberOfFlashes)
  }

  private def increaseEnergyLevels(octopi: Map[Coord, Int]): Map[Coord, Int] = octopi.map { case (coord, value) => coord -> (value + 1) }

  private def flashyFlashyRecurse(octopi: Map[Coord, Int]): (Map[Coord, Int], Int) = {
    def go(octopiWithIndices: Map[Coord, Int], alreadyFlashed: Seq[Coord]): (Map[Coord, Int], Int) = {
      val flashers = octopiWithIndices.filter { case (coord, value) => value > 9 && !alreadyFlashed.contains(coord) }
      flashers.toSeq match {
        case Nil => (octopiWithIndices, alreadyFlashed.size)
        case flasher :: _ =>
          val updatedOctopi = burst(flasher._1, octopiWithIndices)
          go(updatedOctopi, flasher._1 +: alreadyFlashed)

      }
    }
    go(octopi, Nil)
  }

  private def burst(c: Coord, octopi: Map[Coord, Int]): Map[Coord, Int] = {
    val neighbouringCoords =
      adjacentVectors.map { case (xDelta, yDelta) => Coord(c.x + xDelta, c.y + yDelta) }.filter(coord => octopi.contains(coord))
    neighbouringCoords.foldLeft(octopi) { case (octs, c) => octs.updated(c, octs(c) + 1) }
  }

  override def partTwo(l: List[String]): Int = {
    val withIndices = getOctopiWithIndices(l)
    @tailrec
    def goUntilSynchronised(i: Int, octopi: Map[Coord, Int]): Int = {
      val (newOctopi, numberOfFlashes) = tick(octopi)
      if (numberOfFlashes == 100) {
        i
      } else {
        goUntilSynchronised(i + 1, newOctopi)
      }
    }
    goUntilSynchronised(1, withIndices)
  }
}

//
//First, the energy level of each octopus increases by 1.
//Then, any octopus with an energy level greater than 9 flashes.
// This increases the energy level of all adjacent octopuses by 1, including octopuses that are
// diagonally adjacent. If this causes an octopus to have an energy level greater than 9, it also flashes.
// This process continues as long as new octopuses keep having their energy level increased beyond 9.
// (An octopus can only flash at most once per step.)
//Finally, any octopus that flashed during this step has its energy level set to 0, as it used all of its
// energy to flash.

object DayElevenData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "5483143223",
    "2745854711",
    "5264556173",
    "6141336146",
    "6357385478",
    "4167524645",
    "2176841721",
    "6882881134",
    "4846848554",
    "5283751526"
  )
  override val expectedPartOne: Option[Int] = Some(1656)
  override val expectedPartTwo: Option[Int] = Some(195)
}
