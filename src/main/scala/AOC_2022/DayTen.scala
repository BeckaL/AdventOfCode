package AOC_2022

import shared.{DayChallenge, Helpers, TestData}

object DayTen extends DayChallenge[Int, String] with Helpers {
  override def partOne(l: List[String]): Int = {
    val (x, score, time) = (1, 0, 1)
    l.foldLeft((x, score, time)) { case ((currentX, currentScore, currentTime), line) =>
      val updatedScore = updateScoreAtTimes(currentScore, currentTime, currentX)
      extractIntsWithOptionalSigns(line) match {
        case Nil => (currentX, updatedScore, currentTime + 1)
        case i :: Nil =>
          val newCurrentX = currentX + i
          val scoreAfterSecondTick = updateScoreAtTimes(updatedScore, currentTime + 1, currentX)
          (newCurrentX, scoreAfterSecondTick, currentTime + 2)
      }
    }._2
  }

  override def partTwo(l: List[String]): String = {
    val pixels: List[Char] = List.fill(240)('.')
    val resultString = l.foldLeft(1, pixels, 0) { case ((sprite, pixels, time), line) =>
      val updatedPixels = pixelsDuringTick(time, sprite, pixels)
      extractIntsWithOptionalSigns(line) match {
        case Nil => (sprite, updatedPixels, time + 1)
        case i :: Nil => (sprite + i, pixelsDuringTick(time + 1, sprite, updatedPixels), time + 2)
      }
    }._2.mkString("").sliding(40, 40).mkString("\n")
    "\n" + resultString
  }

  private def updateScoreAtTimes(score: Int, time: Int, x: Int) = {
    val timesWanted = Set(20, 60, 100, 140, 180, 220)
    if (timesWanted.contains(time)) score + (x * time) else score
  }

  private def pixelsDuringTick(time: Int, sprite: Int, pixels: List[Char]) =
    if (pixelsCoveredBySprite(sprite).contains(time % 40))
      updatedPixels(pixels, time, '#')
    else
      pixels

  private def pixelsCoveredBySprite(middleX: Int): List[Int] = List(middleX - 1, middleX, middleX + 1)

  private def updatedPixels(p: List[Char], i: Int, char: Char): List[Char] = {
    val (before, after) = p.splitAt(i)
    (before :+ char) ++ after.tail
  }
}

object DayTenData extends TestData[Int, String] {
  override val testData: List[String] = List(
    "addx 15",
    "addx -11",
    "addx 6",
    "addx -3",
    "addx 5",
    "addx -1",
    "addx -8",
    "addx 13",
    "addx 4",
    "noop",
    "addx -1",
    "addx 5",
    "addx -1",
    "addx 5",
    "addx -1",
    "addx 5",
    "addx -1",
    "addx 5",
    "addx -1",
    "addx -35",
    "addx 1",
    "addx 24",
    "addx -19",
    "addx 1",
    "addx 16",
    "addx -11",
    "noop",
    "noop",
    "addx 21",
    "addx -15",
    "noop",
    "noop",
    "addx -3",
    "addx 9",
    "addx 1",
    "addx -3",
    "addx 8",
    "addx 1",
    "addx 5",
    "noop",
    "noop",
    "noop",
    "noop",
    "noop",
    "addx -36",
    "noop",
    "addx 1",
    "addx 7",
    "noop",
    "noop",
    "noop",
    "addx 2",
    "addx 6",
    "noop",
    "noop",
    "noop",
    "noop",
    "noop",
    "addx 1",
    "noop",
    "noop",
    "addx 7",
    "addx 1",
    "noop",
    "addx -13",
    "addx 13",
    "addx 7",
    "noop",
    "addx 1",
    "addx -33",
    "noop",
    "noop",
    "noop",
    "addx 2",
    "noop",
    "noop",
    "noop",
    "addx 8",
    "noop",
    "addx -1",
    "addx 2",
    "addx 1",
    "noop",
    "addx 17",
    "addx -9",
    "addx 1",
    "addx 1",
    "addx -3",
    "addx 11",
    "noop",
    "noop",
    "addx 1",
    "noop",
    "addx 1",
    "noop",
    "noop",
    "addx -13",
    "addx -19",
    "addx 1",
    "addx 3",
    "addx 26",
    "addx -30",
    "addx 12",
    "addx -1",
    "addx 3",
    "addx 1",
    "noop",
    "noop",
    "noop",
    "addx -9",
    "addx 18",
    "addx 1",
    "addx 2",
    "noop",
    "noop",
    "addx 9",
    "noop",
    "noop",
    "noop",
    "addx -1",
    "addx 2",
    "addx -37",
    "addx 1",
    "addx 3",
    "noop",
    "addx 15",
    "addx -21",
    "addx 22",
    "addx -6",
    "addx 1",
    "noop",
    "addx 2",
    "addx 1",
    "noop",
    "addx -10",
    "noop",
    "noop",
    "addx 20",
    "addx 1",
    "addx 2",
    "addx 2",
    "addx -6",
    "addx -11",
    "noop",
    "noop",
    "noop"
  )
  override val expectedPartOne: Option[Int] = Some(13140)
  override val expectedPartTwo: Option[String] = Some(
    "\n##..##..##..##..##..##..##..##..##..##.." +
      "\n###...###...###...###...###...###...###." +
      "\n####....####....####....####....####...." +
      "\n#####.....#####.....#####.....#####....." +
      "\n######......######......######......####" +
      "\n#######.......#######.......#######.....")
}