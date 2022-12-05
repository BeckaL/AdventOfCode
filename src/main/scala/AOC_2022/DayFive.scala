package AOC_2022

import shared.{DayChallenge, TestData, Helpers}

object DayFive extends DayChallenge[String, String] with Helpers {
  type Stacks = Map[Int, List[Char]]

  override def partOne(l: List[String]): String = {
    val stacks :: moves :: _ = splitIntoGroupsOfList(l)
    getTopsOFEndStacks(parseStacks(stacks), moves, move)
  }

  override def partTwo(l: List[String]): String = {
    val stacks :: moves :: _ = splitIntoGroupsOfList(l)
    getTopsOFEndStacks(parseStacks(stacks), moves, moveTwo)
  }

  private def getTopsOFEndStacks(stacks: Stacks, moves: List[String], transformF: (Int, Int, Int, Stacks) => Stacks): String = {
    val endStacks = moves.foldLeft(stacks){case (currentStacks, instruction) =>
      val moveN :: from :: to :: _ = extractInts(instruction)
      transformF(moveN, from, to, currentStacks)
    }
    endStacks.toList.sortBy{case (i, _) => i}.map(_._2.headOption.getOrElse(" ")).mkString("")
  }

  private def parseStacks(l: List[String]): Stacks = {
    val numberOfStacks = (l.head.size + 1) / 4
    val emptyM = (1 to numberOfStacks).map(i => i -> List[Char]()).toMap
    l.reverse.foldLeft(emptyM){case (m, line) => (1 to numberOfStacks).foldLeft(m) { case (innerM, i) =>
      val char = line((i - 1) * 4 + 1)
      if (char != ' ') innerM.updated(i, char +: innerM(i)) else innerM
    }}
  }

  private def move(n: Int, from: Int, to: Int, stacks: Stacks): Stacks =
    (1 to n).foldLeft(stacks){case (s, _) =>
      s.updated(from, s(from).tail).updated(to, s(from).head +: s(to))
    }

  private def moveTwo(n: Int, from: Int, to: Int, stacks: Stacks): Stacks =
    stacks.updated(from, stacks(from).drop(n)).updated(to, stacks(from).take(n) ++ stacks(to))
}

object DayFiveData extends TestData[String, String] {
  override val testData: List[String] = List(
    "    [D]    ",
    "[N] [C]    ",
    "[Z] [M] [P]",
    "1   2   3 ",
    "",
    "move 1 from 2 to 1",
    "move 3 from 1 to 3",
    "move 2 from 2 to 1",
    "move 1 from 1 to 2"
  )
  override val expectedPartOne: Option[String] = Some("CMZ")
  override val expectedPartTwo: Option[String] = Some("MCD")
}