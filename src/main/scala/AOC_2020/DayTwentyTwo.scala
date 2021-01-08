package AOC_2020

import shared.{DayChallenge, Helpers}

import scala.annotation.tailrec

object DayTwentyTwo extends DayChallenge[Long, Long] with Helpers{

  case class GameState(p1: List[Int], p2: List[Int]) {
    def playerOneWins = GameState(p1.tail :+ p1.head :+ p2.head, p2.tail)
  }


  override def partOne(l: List[String]): Long = {
    val (p1Cards, p2Cards) = getP1AndP2Cards(l)

    @tailrec
    def play(s: GameState): List[Int] =
      (s.p1, s.p2) match {
        case (p1Hand, Nil) => p1Hand
        case (Nil, p2Hand) => p2Hand
        case _ => play(resolveRound(s))
      }

    play(GameState(p1Cards, p2Cards)).reverse.zipWithIndex.map{case(card, index) => card * (index + 1)}.sum
  }

  private def resolveRound(s: GameState) =
    (s.p1.head, s.p2.head) match {
      case (p1Card, p2Card) if (p1Card > p2Card) =>  s.playerOneWins
      case (p1Card, p2Card) => GameState(s.p1.tail, s.p2.tail :+ p2Card :+ p1Card)
    }

  private def getP1AndP2Cards(l: List[String]): (List[Int], List[Int]) = {
    val (player1, player2) = getTwoFromSplit(l.mkString("\n"), "\n\n")
    (player1.split("\n").tail.map(_.toInt).toList, player2.split("\n").tail.map(_.toInt).toList)
  }

 def recursePlay(s: GameState, previousStates: List[GameState], game: Int): (List[Int], List[Int]) = {
    if (previousStates contains (s)) {
      (s.p1, List())
    } else {
      if (s.p1.isEmpty || s.p2.isEmpty) {
        (s.p1, s.p2)
      } else {
        val p1Card = s.p1.head
        val p2Card = s.p2.head
        val (newP1Hand, newP2Hand) = if (p1Card <= s.p1.size - 1 && p2Card <= s.p2.size - 1) {
          val recursiveState = recursePlay(GameState(s.p1.tail.take(p1Card), s.p2.tail.take(p2Card)), List(), game + 1)
          val winner = if (recursiveState._1.isEmpty) 2 else 1
          if (winner == 1) {
            (s.p1.tail :+ p1Card :+ p2Card, s.p2.tail)
          } else {
            (s.p1.tail, s.p2.tail :+ p2Card :+ p1Card)
          }
        } else {
          val newS = resolveRound(s)
          (newS.p1, newS.p2)
        }
        recursePlay(GameState(newP1Hand, newP2Hand), GameState(s.p1, s.p2) +: previousStates, game)
      }
    }
  }

  override def partTwo(l: List[String]): Long = {
    val (p1Cards, p2Cards) = getP1AndP2Cards(l)
    val endState = recursePlay(GameState(p1Cards, p2Cards), List(), 0)
    (endState._1 ++ endState._2).reverse.zipWithIndex.map{case(card, index) => card * (index + 1)}.sum
  }


  override val expectedPartOne: Option[Long] = Some(306)
  override val expectedPartTwo: Option[Long] = Some(291)
  override val testData: List[String] = List(
      "Player 1:",
      "9",
      "2",
      "6",
      "3",
      "1",
      "",
      "Player 2:",
      "5",
      "8",
      "4",
      "7",
      "10")

}