package AOC_2023

import shared.{DayChallenge, Helpers, TestData}

object DaySeven extends DayChallenge[Long, Long] with Helpers {
  override def partOne(l: List[String]): Long =
    val cardsOrderedForRanking = List('2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A')
    scoreHand(l, cardsOrderedForRanking, getHandType)

  override def partTwo(l: List[String]): Long =
    val cardsOrderedForRanking = List('J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A')
    scoreHand(l, cardsOrderedForRanking, getHandTypeReplacingJokers)

  private def getHandType(s: String): HandType =
    val charsAndCharacterCountsDescending = s.toSet.toList.map(char => char -> s.count(_ == char)).sorted.reverse
    s.toSet.toList.map(char => s.count(_ == char)).sorted.reverse match
      case 5 :: _ => FiveOfAKind
      case 4 :: _ => FourOfAKind
      case 3 :: 2 :: _ => FullHouse
      case 3 :: _ => ThreeOfAKind
      case 2 :: 2 :: _ => TwoPair
      case 2 :: _ => OnePair
      case _ => HighCard

  private def scoreHand(l: List[String], orderedCards: List[Char], stringToHandType: String => HandType) =
    val orderedHandTypes = l
      .map(getTwoFromSplit(_, " "))
      .map((s, score) => (s, stringToHandType(s), score))
      .sortBy { case (hand, handType, _) => (handType.rank, secondaryRank(hand, orderedCards)) }
    orderedHandTypes.zipWithIndex.map { case ((_, _, score), index) => score.toLong * (index + 1) }.sum

  private def secondaryRank(hand: String, cards: List[Char]) =
    hand.map(c => padDigit(cards.indexOf(c), 2)).mkString("").toInt

  private def getHandTypeReplacingJokers(s: String): HandType =
    val charsInDescendingOrderOfPrevalence =
      s.toSet.toList.map(char => char -> s.count(_ == char)).sortBy(_._2).reverse.map(_._1)
    val replaceCharacter = charsInDescendingOrderOfPrevalence.filterNot(_ == 'J').headOption
    replaceCharacter match
      case Some(c) =>
        val newS = s.replaceAll("J", c.toString)
        getHandType(newS)
      case None => getHandType(s)
}

trait HandType { val rank: Int }
case object FiveOfAKind extends HandType { val rank = 7 }
case object FourOfAKind extends HandType { val rank = 6 }
case object FullHouse extends HandType { val rank = 5 }
case object ThreeOfAKind extends HandType { val rank = 4 }
case object TwoPair extends HandType { val rank = 3 }
case object OnePair extends HandType { val rank = 2 }
case object HighCard extends HandType { val rank = 1 }

object DaySevenData extends TestData[Long, Long] {
  override val testData: List[String] = List(
    "32T3K 765",
    "T55J5 684",
    "KK677 28",
    "KTJJT 220",
    "QQQJA 483"
  )
  override val expectedPartOne: Option[Long] = Some(6440)
  override val expectedPartTwo: Option[Long] = Some(5905)
}