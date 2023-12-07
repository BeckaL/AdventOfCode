package AOC_2023

import shared.{DayChallenge, Helpers, TestData}

object DaySeven extends DayChallenge[Int, Int] with Helpers {
  override def partOne(l: List[String]): Int =
    val orderedHandTypes = l
      .map(getTwoFromSplit(_, " "))
      .map((s, score) => (s, getHandType(s), score))
      .sortBy{case (hand, handType, _) => (handType.rank, secondaryRank(hand))}
      .reverse
    orderedHandTypes.zipWithIndex.map{ case ((_, _, score), index) =>  score.toInt * (index + 1)}.sum


  private def secondaryRank(hand: String) =
    val possibleCards = List('A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2')
    hand.map(card => possibleCards.indexOf(card)).zipWithIndex.map((cardIndex, handIndex) =>
      ((possibleCards.size - cardIndex) * Math.pow(10, 4 - handIndex)).toInt * -1
    ).sum

  private def getHandType(s: String): HandType =
    s.toSet.toList.map(char => s.count(_ == char)).sorted.reverse match
      case 5 :: _ => FiveOfAKind
      case 4 :: 1 :: _  => FourOfAKind
      case 3 :: 2 :: _ => FullHouse
      case 3 :: _ => ThreeOfAKind
      case 2 :: 2 :: _ => TwoPair
      case 2 :: _ => OnePair
      case _ => HighCard

  override def partTwo(l: List[String]): Int =
    2
}

trait HandType { val rank: Int }
case object FiveOfAKind extends HandType { val rank = 1 }
case object FourOfAKind extends HandType { val rank = 2 }
case object FullHouse extends HandType { val rank = 3 }
case object ThreeOfAKind extends HandType { val rank = 4 }
case object TwoPair extends HandType { val rank = 5 }
case object OnePair extends HandType { val rank = 6 }
case object HighCard extends HandType { val rank = 7 }



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