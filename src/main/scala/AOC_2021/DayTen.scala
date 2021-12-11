package AOC_2021

import shared.{DayChallenge, TestData}

object DayTen extends DayChallenge[Int, Long] {
  override def partOne(l: List[String]): Int =
    getScores(l.map(findFirstIllegalCharacter))

  private def getScores(firstIllegalChars: List[Option[Char]]) =
    firstIllegalChars.flatMap(_.toList).map(Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)).sum

  private val validPairs = Map('[' -> ']', '(' -> ')', '{' -> '}', '<' -> '>')

  private def findFirstIllegalCharacter(line: String): Option[Char] = {
    def go(i: Int, unterminatedQueue: List[Char]): Option[Char] = {
      if (!line.indices.contains(i)) {
        None
      } else {
        val char = line(i)
        if (validPairs.keys.toList.contains(char)) {
          go(i + 1, char +: unterminatedQueue)
        } else {
          unterminatedQueue match {
            case mostRecentUnterminatedChar :: others =>
              if (char == validPairs(mostRecentUnterminatedChar)) {
                go(i + 1, others)
              } else { Some(char) }
            case Nil => Some(char)
          }
        }
      }
    }
    go(0, Nil)
  }

  private def completeLine(line: String): List[Char] = {
    def getUnterminated(i: Int, unterminatedQueue: List[Char]): Option[List[Char]] = {
      if (!line.indices.contains(i)) {
        Some(unterminatedQueue)
      } else {
        val char = line(i)
        if (validPairs.keys.toList.contains(char)) {
          getUnterminated(i + 1, char +: unterminatedQueue)
        } else {
          unterminatedQueue match {
            case mostRecentUnterminatedChar :: others if char == validPairs(mostRecentUnterminatedChar) =>
              getUnterminated(i + 1, others)
            case _ => None
          }
        }
      }
    }
    getUnterminated(0, Nil).toList.flatten.map(validPairs)
  }

  def getScore(chars: List[Char]): Long = {
    val scoreMap = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)
    chars.foldLeft(0L) { case (currentScore, char) =>
      currentScore * 5L + scoreMap(char)
    }
  }

  override def partTwo(l: List[String]): Long = {
    val sortedScores = l.map(completeLine)
      .filterNot(line => line.isEmpty)
      .map(unterminated => getScore(unterminated))
      .sorted
    sortedScores(sortedScores.size / 2)
  }
}

object DayTenData extends TestData[Int, Long] {
  override val testData: List[String] = List(
    "[({(<(())[]>[[{[]{<()<>>",
    "[(()[<>])]({[<{<<[]>>(",
    "{([(<{}[<>[]}>{[]{[(<()>",
    "(((({<>}<{<{<>}{[]{[]{}",
    "[[<[([]))<([[{}[[()]]]",
    "[{[{({}]{}}([{[{{{}}([]",
    "{<[[]]>}<{[{[{[]{()[[[]",
    "[<(<(<(<{}))><([]([]()",
    "<{([([[(<>()){}]>(<<{{",
    "<{([{{}}[<[[[<>{}]]]>[]]"
  )
  override val expectedPartOne: Option[Int]  = Some(26397)
  override val expectedPartTwo: Option[Long] = Some(288957L)
}
