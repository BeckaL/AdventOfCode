package AOC_2020

import shared.{DayChallenge, Helpers}

object DayEight extends DayChallenge[Int, Int] with Helpers {

  override def partOne(l: List[String]): Int =
    findAccumulatorForInfiniteLoop(toInstructions(l))

  private def findAccumulatorForInfiniteLoop(instructions: List[(String, Int)], currentI: Int = 0, currentAcc: Int = 0, traversed: List[Int] = List()): Int = {
    val (nextI, nextAcc) = getNextIAndNextAcc(instructions(currentI), currentI, currentAcc)
    if (traversed contains nextI) {
      nextAcc
    } else {
      findAccumulatorForInfiniteLoop(instructions, nextI, nextAcc, traversed :+ currentI)
    }
  }

  private def toInstructions(l: List[String]): List[(String, Int)] =
    l.map(getTwoFromSplit(_, " ")).map(tup => (tup._1, interpretNumberWithSign(tup._2)))

  override def partTwo(l: List[String]): Int =
    findAccumulatorForCorrectModifiedInstruction(toInstructions(l), 0)

  private def findAccumulatorForCorrectModifiedInstruction(instructions: List[(String, Int)], i: Int): Int = {
    val resultFromModifiedL = instructions(i) match {
      case ("nop", acc) => findAccIfTerminatesOnLineAfter(instructions.updated(i, ("jmp", acc)))
      case ("jmp", _) => findAccIfTerminatesOnLineAfter(instructions.updated(i, ("nop", 0)))
      case ("acc", _) => None
    }
    resultFromModifiedL match {
      case Some(i) => i
      case None => findAccumulatorForCorrectModifiedInstruction(instructions, i + 1)
    }
  }

  private def findAccIfTerminatesOnLineAfter(instructions: List[(String, Int)], currentI: Int = 0, currentAcc: Int = 0, traversed: List[Int] = List()): Option[Int] = {
    if (traversed contains currentI) {
      None
    } else if (currentI == instructions.size) {
      Some(currentAcc)
    } else if (!instructions.indices.contains(currentI)) {
      None
    } else {
      val (nextI, nextAcc) = getNextIAndNextAcc(instructions(currentI), currentI, currentAcc)
      findAccIfTerminatesOnLineAfter(instructions, nextI, nextAcc, traversed :+ currentI)
    }
  }

  private def getNextIAndNextAcc(instruction: (String, Int), currentI: Int, currentAcc: Int) =
    instruction match {
      case ("nop", _) => (currentI + 1, currentAcc)
      case ("jmp", acc) => (currentI + acc, currentAcc)
      case ("acc", acc) => (currentI + 1, currentAcc + acc)
    }

  override val expectedPartTwo: Option[Int] = Some(8)
  override val expectedPartOne: Option[Int] = Some(5)
  override val testData: List[String] = List(
    "nop +0",
    "acc +1",
    "jmp +4",
    "acc +3",
    "jmp -3",
    "acc -99",
    "acc +1",
    "jmp -4",
    "acc +6")
}
