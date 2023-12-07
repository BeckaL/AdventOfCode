package AOC_2023

import shared.{DayChallenge, Helpers, TestData}

object DaySeven extends DayChallenge[Long, Int] with Helpers {
  override def partOne(l: List[String]): Long =
    val orderedHandTypes = l
      .map(getTwoFromSplit(_, " "))
      .map((s, score) => (s, getHandType(s), score))
      .sortBy{case (hand, handType, _) => (handType.rank, secondaryRank(hand))}
    orderedHandTypes.zipWithIndex.map{ case ((_, _, score), index) =>  score.toLong * (index + 1)}.sum

  private def secondaryRank(hand: String) =
    val possibleCards = List('2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A')
    hand.map(c => padDigit(possibleCards.indexOf(c), 2)).mkString("").toInt

  private def getHandType(s: String): HandType =
    s.toSet.toList.map(char => s.count(_ == char)).sorted.reverse match
      case 5 :: _ => FiveOfAKind
      case 4 :: _  => FourOfAKind
      case 3 :: 2 :: _ => FullHouse
      case 3 :: _ => ThreeOfAKind
      case 2 :: 2 :: _ => TwoPair
      case 2 :: _ => OnePair
      case _ => HighCard

  override def partTwo(l: List[String]): Int =
    2
}

trait HandType { val rank: Int }
case object FiveOfAKind extends HandType { val rank = 7 }
case object FourOfAKind extends HandType { val rank = 6 }
case object FullHouse extends HandType { val rank = 5 }
case object ThreeOfAKind extends HandType { val rank = 4 }
case object TwoPair extends HandType { val rank = 3 }
case object OnePair extends HandType { val rank = 2 }
case object HighCard extends HandType { val rank = 1 }



object DaySevenData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "32T3K 765",
    "T55J5 684",
    "KK677 28",
    "KTJJT 220",
    "QQQJA 483"
  )
  override val expectedPartOne: Option[Int] = Some(6440)
  override val expectedPartTwo: Option[Int] = Some(0)
}