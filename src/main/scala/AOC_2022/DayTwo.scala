package AOC_2022

import shared.{DayChallenge, Helpers, TestData}

object DayTwo extends DayChallenge[Int, Int] with Helpers{
  override def partOne(l: List[String]): Int =
    l.map(getTwoFromSplit(_, " "))
      .map{case (p1, p2) => playerTwoScore((stringToRps(p1), stringToRps(p2)))}
      .sum

  override def partTwo(l: List[String]): Int =
    l.map(getTwoFromSplit(_, " "))
      .map{ case (p1, r) => getScoreOfIntendedResult(stringToRps(p1), r)}
      .sum

  def playerTwoScore(moves: (RPS, RPS)): Int  =
    moves match {
      case (p1, p2) if p2.winsAgainst == p1 => 6 + p2.score
      case (p1, p2) if p1 == p2 => 3 + p2.score
      case (_, p2) => p2.score
    }

  private def getScoreOfIntendedResult(p1: RPS, intendedResult: String): Int =
    intendedResult match {
      case "Y" => playerTwoScore((p1, p1))
      case "Z" => playerTwoScore((p1, p1.losesAgainst))
      case _ => playerTwoScore((p1, p1.winsAgainst))
    }

  private def stringToRps(str: String): RPS =
    Map("X" -> Rock, "Y" -> Paper, "Z" -> Scissors, "A" -> Rock, "B" -> Paper, "C" -> Scissors)(str)

  trait RPS { val losesAgainst: RPS; val winsAgainst: RPS; val score: Int }
  case object Rock extends RPS { val losesAgainst = Paper; val winsAgainst = Scissors; val score = 1 }
  case object Paper extends RPS { val losesAgainst = Scissors; val winsAgainst = Rock; val score = 2 }
  case object Scissors extends RPS { val losesAgainst = Rock; val winsAgainst = Paper; val score = 3 }

}

object DayTwoData extends TestData[Int, Int] {
  override val testData: List[String] = List("A Y", "B X", "C Z")
  override val expectedPartOne: Option[Int] = Some(15)
  override val expectedPartTwo: Option[Int] = Some(12)
}
