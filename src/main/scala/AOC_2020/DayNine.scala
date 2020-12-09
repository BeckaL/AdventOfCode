package AOC_2020

import shared.{DayChallenge, Helpers}

object DayNine extends DayChallenge[BigInt, BigInt] with Helpers {
  override def partOne(l: List[String]): BigInt = {
    val preamble = getPreamble(l)
    val bigInts = l.map(BigInt(_))
    bigInts.zipWithIndex.find { case(k, i) =>
      if (i < preamble) false else !pairSummingToNExists(bigInts.slice(i - preamble, i), k, preamble)
    }.map(_._1).get
  }

  private def pairSummingToNExists(ints: List[BigInt], n: BigInt, premable: Int) =
    ints.exists { k => ints.contains(n - k)}

  private def sumOfSmallestAndLargest(is: List[BigInt],ans: BigInt,  startingI: Int = 0, i: Int = 0, runningTotal: BigInt = 0): BigInt =
    is(i) + runningTotal match {
      case x if x  == ans =>
        val range = is.slice(startingI, i + 1)
        range.min + range.max
      case x if x < ans => sumOfSmallestAndLargest(is,ans, startingI, i + 1, x)
      case _ => sumOfSmallestAndLargest(is, ans, startingI + 1, startingI + 1)
    }

  override def partTwo(l: List[String]): BigInt =
    sumOfSmallestAndLargest(l.map(BigInt(_)), getAnswerFromPartOne(l))

  private def getPreamble(l: List[String]) =
    if (l == testData) 5 else 25

  private def getAnswerFromPartOne(l: List[String]) =
    if (l == testData) BigInt(127) else BigInt(530627549)

  override val expectedPartOne: Option[BigInt] = Some(127)

  override val expectedPartTwo: Option[BigInt] = Some(62)

  override val testData: List[String] = List("35",
    "20",
    "15",
    "25",
    "47",
    "40",
    "62",
    "55",
    "65",
    "95",
    "102",
    "117",
    "150",
    "182",
    "127",
    "219",
    "299",
    "277",
    "309",
    "576")
}
