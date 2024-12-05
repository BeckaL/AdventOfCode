package AOC_2024

import shared.{DayChallenge, Helpers, TestData}

import scala.annotation.tailrec

object DayFive extends DayChallenge[Int, Int] with Helpers {
  override def partOne(l: List[String]): Int =
    val (rulesLines, pagesLines) = l.splitAt(l.indexOf(""))
    val pagesLists = pagesLines.tail.map(_.split(",").map(_.toInt).toList)
    val rules = parseRules(rulesLines)
    pagesLists.filter(isValidPage(_, rules)).map(l => l(l.size / 2)).sum

  override def partTwo(l: List[String]): Int =
    2

  private def isValidPage(pages: List[Int], rules: Map[Int, Set[Int]]): Boolean =
    @tailrec
    def go(soFar: Set[Int], remaining: List[Int]): Boolean =
      remaining match
        case Nil => true
        case firstInt :: others =>
          val maybeRule = rules.get(firstInt)
          maybeRule match
            case Some(cantBePrecededBy) if cantBePrecededBy.intersect(soFar).nonEmpty => false
            case _ => go(soFar + firstInt, others)
    go(Set(), pages)

  private def parseRules(l: List[String]) =
    l.map { s =>
      val split = s.split("\\|")
      (split(0).toInt, split(1).toInt)
    }.groupMap(_._1)(_._2).map { case (i, mapsTo) => (i, mapsTo.toSet) }
}

object DayFiveData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "47|53",
    "97|13",
    "97|61",
    "97|47",
    "75|29",
    "61|13",
    "75|53",
    "29|13",
    "97|29",
    "53|29",
    "61|53",
    "97|53",
    "61|29",
    "47|13",
    "75|47",
    "97|75",
    "47|61",
    "75|61",
    "47|29",
    "75|13",
    "53|13",
    "",
    "75,47,61,53,29",
    "97,61,53,29,13",
    "75,29,13",
    "75,97,47,61,53",
    "61,13,29",
    "97,13,75,29,47"
  )
  override val expectedPartOne: Option[Int] = Some(143)
  override val expectedPartTwo: Option[Int] = Some(123)
}