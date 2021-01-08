package AOC_2020

import shared.{DayChallenge, Helpers}

import scala.annotation.tailrec

object DayTwentyThree extends DayChallenge[String, Long] with Helpers {
  private val tenMillion = 10000000
  private val oneMillion = 1000000
  override val expectedPartOne: Option[String] = Some("67384529")
  override val expectedPartTwo: Option[Long] = Some("149245887792".toLong)
  override val testData: List[String] = List("389125467")
  override def partOne(l: List[String]): String = {
    val cups = getCups(l)
    from1(CrabIterator(getMap(cups), 100, cups).go())
  }
  private def getCups(l: List[String]) = l.head.split("").map(_.toInt).toList

  override def partTwo(l: List[String]): Long = {
    val cups = getCups(l)
    val allCups = cups ++ ((cups.max + 1) to 1000000)
    val endState = CrabIterator(getMapToOneMillion(cups), tenMillion, allCups).go()
    twoClockwiseFrom1(endState)
  }

  private def getMapToOneMillion(cups: List[Int])= {
    val m = getMap(cups) + (cups.last -> 10)
    m ++ (10 until oneMillion).map { i => i -> (i + 1) }.toMap + (oneMillion -> cups.head)
  }

  private def getMap(cups: List[Int]): Map[Int, Int] = cups.zipWithIndex.map { case (c, i) => c -> cups(cycle(i, cups.size)) }.toMap

  case class CrabIterator(startM: Map[Int, Int], numberOfIterations: Long, cups: List[Int]) {
    val min = cups.min
    val max = cups.max
    val startCup = cups.head
    @tailrec
    final def go(m: Map[Int, Int] = startM, i: Int = 1, currentCup: Int = startCup): Map[Int, Int] = {
      val pickedUp1 = m(currentCup)
      val pickedUp2 = m(pickedUp1)
      val pickedUp3 = m(pickedUp2)
      val destination = findDestinationCupValue(currentCup, List(pickedUp1, pickedUp2, pickedUp3), cups, min, max)
      val newM = m + (currentCup -> m(pickedUp3)) + (pickedUp3 -> m(destination)) + (destination -> m(currentCup))
      if (i == numberOfIterations) newM else go(newM, i + 1, newM(currentCup))
    }
  }


  private def from1(cups: Map[Int, Int], l: String = "", current: Int = 1, iteration: Int = 1): String = {
    val newN = cups(current)
    if (newN == 1 || iteration == 10 ) l else from1(cups, l + newN.toString, newN, iteration + 1)
  }

  private def twoClockwiseFrom1(cs: Map[Int, Int]): Long = cs(1).toLong * cs(cs(1)).toLong

  private def findDestinationCupValue(currentCupValue: Int, pickedUp: List[Int], cups: List[Int], min: Int, max: Int): Int = {
    val newCup = currentCupValue - 1
    def go(i: Int): Int =
      if (pickedUp contains i) go(i - 1) else if (i < min) go(max) else i
    go(newCup)
  }
}
