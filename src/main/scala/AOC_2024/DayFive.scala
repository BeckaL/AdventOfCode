package AOC_2024

import shared.{DayChallenge, Helpers, TestData}

object DayFive extends DayChallenge[Int, Int] with Helpers {
  override def partOne(l: List[String]): Int =
    getPagesAndSortedPages(l).collect{ case (page, sorted) if page == sorted => page }
      .map(l => l(l.size / 2)).sum

  override def partTwo(l: List[String]): Int =
    getPagesAndSortedPages(l).collect { case (page, sorted) if page != sorted => sorted }
      .map(l => l(l.size / 2)).sum

  private def getPagesAndSortedPages(l: List[String]): List[(List[Int], List[Int])] =
    val (rulesLines, pagesLines) = l.splitAt(l.indexOf(""))
    val pagesLists = pagesLines.tail.map(_.split(",").map(_.toInt).toList)
    val ordering = myOrdering(parseRules(rulesLines))
    pagesLists.map(page => (page, page.sorted(ordering)))

  private def parseRules(l: List[String]) =
    l.map { s =>
      val split = s.split("\\|")
      (split(0).toInt, split(1).toInt)
    }.groupMap(_._1)(_._2).map { case (i, mapsTo) => (i, mapsTo.toSet) }

  def myOrdering(m: Map[Int, Set[Int]]):  Ordering[Int] =
    (a: Int, b: Int) =>
      (m.get(b), m.get(a)) match
        case (Some(greaterThanBs), _) if greaterThanBs.contains(a) => 1
        case (_, Some(greaterThanAs)) if greaterThanAs.contains(b) => -1
        case _ => 0
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