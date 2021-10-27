package AOC_2017

import shared.{Coord, DayChallenge, TestData}

object DayThree extends DayChallenge[Int, Int] {
  override def partOne(l: List[String]): Int = getSpiralNumber(l.head.toInt)

  private def sumFactorial(i: Int) = if (i == 0) 0 else (1 to i).toList.sum

  private def getSpiralNumber(i: Int, currentSpiralNumber: Int = 1, maxFromPreviousSpiral: Int = 1): Int = {
    1 + 8 * sumFactorial(currentSpiralNumber) match {
      case maxForCurrentSpiral if maxForCurrentSpiral >= i =>
        val side = (currentSpiralNumber until currentSpiralNumber * 2).toList.reverse.tail ++ (currentSpiralNumber to currentSpiralNumber * 2).toList
        (side ++ side ++ side ++ side).zipWithIndex.find { case (distance, index) => maxFromPreviousSpiral + index + 1 == i }.map(_._1).get
      case _ =>
        getSpiralNumber(i, currentSpiralNumber + 1, 1 + 8 * sumFactorial(currentSpiralNumber))
    }
  }

  case class SpiralGrid(rows: List[List[Option[Int]]]) {
    def get(c: Coord): Option[Int] = {
      import c._
      if (rows.indices.contains(y) && rows.head.indices.contains(x))
        rows(y)(x)
      else None
    }
    def getNeighbours(c: Coord): List[Int] = {
      import c._
      val neighbouringCoords = (for {
       x <- (x - 1 to x + 1)
       y <- (y - 1 to y + 1)
      } yield Coord(x, y)).filterNot(coord => coord == c)
      neighbouringCoords.toList.flatten(coord => get(coord).toList)
    }

    def updateAt(c: Coord, value: Int): SpiralGrid =
      SpiralGrid(rows.updated(c.y, rows(c.y).updated(c.x, Some(value))))

    override def toString: String = rows.map(row => row.map(i => i.getOrElse(" ").toString).mkString(" ")).mkString("\n")
  }

  object SpiralGrid {
    def expand(g: SpiralGrid): SpiralGrid = {
      val newLineSize = g.rows.size + 2
      val firstAndLastRow = List.fill(newLineSize)(None)
      val middleRows = g.rows.map(row => None +: row :+ None)
      SpiralGrid(firstAndLastRow +: middleRows :+ firstAndLastRow)
    }

    def placeNewSpiral(g: SpiralGrid): (SpiralGrid, List[Int]) = {
      val expandedSpiral = SpiralGrid.expand(g)
      val lastXandY = expandedSpiral.rows.indices.max
      val firstCoord = Coord(lastXandY, lastXandY - 1)
      val lastCoord = Coord(lastXandY, lastXandY)

      def next(current: Coord, g: SpiralGrid): Coord = {
        import current._
        val max = g.rows.indices.max

        val min = 0
        if (x == max) {
          if (y > 0) Coord(x, y - 1) else Coord(x - 1, y)
        } else if (x == min) {
          if (y < max) Coord(x, y + 1) else Coord(x + 1, y)
        } else if (y == max) Coord(x + 1, y) else Coord(x - 1, y)
      }

      def go(toFill: Coord, currentG: SpiralGrid, updatedSoFar: List[Int]): (SpiralGrid, List[Int]) = {
        val newCellValue = currentG.getNeighbours(toFill).sum
        val newGrid = currentG.updateAt(toFill, newCellValue)
        val newUpdatedSoFar = newCellValue +: updatedSoFar
        if (toFill == lastCoord) {
          (newGrid, newUpdatedSoFar)
        } else {
          go(next(toFill, currentG), newGrid, newUpdatedSoFar)
        }
      }

      go(firstCoord, expandedSpiral, List.empty)
    }
  }

  override def partTwo(l: List[String]): Int = {
    val initialGrid = SpiralGrid(List(List(Some(1))))
    val i = l.head.toInt

    def goUntilMeets(g: SpiralGrid): Int = {
      val (newSpiral, newPlacedValues) = SpiralGrid.placeNewSpiral(g)
      val maybePlacedBiggerThanI = newPlacedValues.filter(k => k > i)
      maybePlacedBiggerThanI match {
        case Nil => goUntilMeets(newSpiral)
        case i :: is => (i :: is).min
      }
    }

    goUntilMeets(initialGrid)
  }
}

object DayThreeData extends TestData[Int, Int] {
  override val testData: List[String] = List("1024")
  override val expectedPartOne: Option[Int] = Some(31)

  override val testData2: Option[List[String]] = Some(List("800"))
  override val expectedPartTwo: Option[Int] = Some(806)
}
