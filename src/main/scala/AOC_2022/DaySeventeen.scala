package AOC_2022

import shared.UpdaterHelpers.ListStringUpdater
import shared.{Coord, DayChallenge, TestData, UpdaterHelpers}

object DaySeventeen extends DayChallenge[Int, Long] {
  override def partOne(l: List[String]): Int = sizeAfterNIterations(l.head, 2022)

  override def partTwo(l: List[String]): Long = {
    val (firstSeenAt, diff, sizeAtFirstSeenAt, cycleLength) = findRepeat(l.head, 0, 0, Map(), Nil)
    val multiplier = (1000000000000L - firstSeenAt) / cycleLength
    val extra = (1000000000000L - firstSeenAt) % cycleLength
    val sizeAtExtra = sizeAfterNIterations(l.head, firstSeenAt + extra.toInt)
    val toAdd = sizeAtExtra - sizeAtFirstSeenAt
    sizeAtFirstSeenAt.toLong + multiplier * diff + toAdd
  }

  private def sizeAfterNIterations(instructions: String, iterations: Int): Int = {
    def go(shapeCounter: Int, instructionCounter: Int, map: List[String]): Int =
      if (shapeCounter == iterations)
        map.count(line => line != ".......")
      else {
        val (newShape, newMap) = produceShapeAndMap(shapeCounter, map)
        val (mapAfterShapeHasFallen, newInstructionCounter) = moveUntilSettled(newShape, instructions, newMap, instructionCounter)
        go(shapeCounter + 1, newInstructionCounter, mapAfterShapeHasFallen)
      }

    go(0, 0, Nil)
  }

  private def moveUntilSettled(shape: List[Coord], instructions: String, map: List[String], instructionCounter: Int): (List[String], Int) = {
    val xMove = if (instructions(instructionCounter % instructions.length) == '<') -1 else 1
    val (shapeAfterMoveSideways, _) = move(xMove, 0, shape, map)
    val (shapeAfterDrop, moved) = move(byX = 0, byY = 1, shapeAfterMoveSideways, map)
    if (!moved)
      (shapeAfterDrop.foldLeft(map) { case (updatedMap, shapeCoord) =>
        updatedMap.update(shapeCoord.x, shapeCoord.y, '#') }, instructionCounter + 1
      )
    else
      moveUntilSettled(shapeAfterDrop, instructions, map, instructionCounter + 1)
  }

  private def move(byX: Int, byY: Int, shape: List[Coord], map: List[String]): (List[Coord], Boolean) = {
    val newCoords = shape.map { case Coord(x, y) => Coord(x + byX, y + byY) }
    if (newCoords.exists(c => cellAtSafe(map, c.x, c.y).isEmpty || map(c.y)(c.x) == '#')) (shape, false) else (newCoords, true)
  }

  private def cellAtSafe(map: List[String], x: Int, y: Int): Option[Char] =
    if (map.indices.contains(y) && map.head.indices.contains(x)) Some(map(y)(x)) else None

  private def produceShapeAndMap(counter: Int, map: List[String]): (List[Coord], List[String]) = {
    val shape = counter % 5 match {
      case 0 => (2 until 6).map((_, 0)).toList
      case 1 => (3, 2) +: (2 until 5).map((_, 1)).toList :+ (3, 0)
      case 2 => (4, 0) +: ((4, 1) +: (2 until 5).map((_, 2)).toList)
      case 3 => (0 until 4).toList.map(yD => (2, yD))
      case 4 => (0 until 2).toList.flatMap(xD => List((2 + xD, 0), (2 + xD, 1)))
    }
    val shapeHeight = shape.map(_._2).max - shape.map(_._2).min
    val newMap = List.fill(3 + shapeHeight + 1)(".......") ++ map.takeRight(map.size - findLowestY(map))
    (shape.map(Coord.from), newMap)
  }

  private def findRepeat(instructions: String, shapeCounter: Int, instructionCounter: Int, m: Map[String, (Int, Int)], map: List[String]): (Int, Int, Int, Int) = {
    val (newShape, newMap) = produceShapeAndMap(shapeCounter, map)
    val (updatedMap, newInstructionCounter) = moveUntilSettled(newShape, instructions, newMap, instructionCounter)
    val size = map.count(line => line != ".......")
    if (size > 50) {
      val previous50 = updatedMap.slice(0, 50).mkString("\n")
      m.get(previous50) match {
        case Some((lastI, prevSize)) => (lastI, size - prevSize, prevSize, shapeCounter - lastI)
        case None => findRepeat(instructions, shapeCounter + 1, newInstructionCounter, m.updated(previous50, (shapeCounter, size)), updatedMap)
      }
    } else
      findRepeat(instructions, shapeCounter + 1, newInstructionCounter, m, updatedMap)
  }

  private def findLowestY(map: List[String]): Int =
    map.zipWithIndex.find { case (s, _) => s.contains('#') }.map(_._2).getOrElse(-1)
}

object DaySeventeenData extends TestData[Int, Long] {
  override val testData: List[String] = List(
    ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
  )
  override val expectedPartOne: Option[Int] = Some(3068)
  override val expectedPartTwo: Option[Long] = Some(1514285714288L)
}