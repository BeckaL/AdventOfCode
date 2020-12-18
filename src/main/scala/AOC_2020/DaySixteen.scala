package AOC_2020

import shared.{DayChallenge, Helpers}

object DaySixteen extends DayChallenge[Int, Long] with Helpers {

  override def partOne(l: List[String]): Int = {
    val state = State.from(l)
    state.otherTickets.flatMap(findFieldsWhichCannotBeValid(_, state.rules)).sum
  }

  override def partTwo(l: List[String]): Long = {
    val state = State.from(l)
    resolveCandidates(getCandidateRules(state, getValidTickets(state)))
      .collect { case (index, rule) if rule.ruleName.startsWith("departure") => index }
      .map(i => state.myTicket(i).toLong)
      .product
  }

  private def findFieldsWhichCannotBeValid(ticket: List[Int], rules: List[Rule]): List[Int] =
    ticket.filter(field => rules.forall(r => !r.isValid(field)))

  private def findCandidatesForField(values: List[Int], rules: List[Rule]): List[Rule] =
    rules.filter { r => values.forall {r.isValid} }

  case class State(rules: List[Rule], myTicket: List[Int], otherTickets: List[List[Int]])

  object State {
    def from(l: List[String]) = {
      val (ruleLines, ticketLines) = l.splitAt(l.indexOf(""))
      val rules = getRules(ruleLines)
      val (myTicketLines, otherTicketLines) = ticketLines.tail.splitAt(ticketLines.tail.indexOf(""))
      val myTicket = getTicket(myTicketLines(1))
      val otherTickets = otherTicketLines.drop(2).map(getTicket)
      State(rules, myTicket, otherTickets)
    }
  }

  private def getTicket(ticketString: String): List[Int] =
    ticketString.split(",").map(_.toInt).toList

  private def getRules(ruleLines: List[String]): List[Rule] =
    ruleLines.map { line =>
      val (ruleName, ranges) = getTwoFromSplit(line, ": ")
      Rule(ruleName, ranges.split(" or ").toList.map(getRange))
    }

  private def getRange(s: String): Range = {
    val (start, end) = getTwoFromSplit(s, "-")
    start.toInt to end.toInt
  }

  def resolveCandidates(candidates: List[Candidate]): Map[Int, Rule] = {
    if (candidates.count(_.isResolved) == candidates.size) {
      candidates.map(_.actual).toMap
    } else {
      val (resolved, unresolved) = candidates.partition(_.isResolved)
      val resolvedRules = resolved.flatMap(_.possibles)
      val newCandidates = unresolved.map(c => c.copy(possibles = c.possibles.filterNot {resolvedRules contains _ }))
      resolveCandidates(newCandidates ++ resolved)
    }
  }

  case class Candidate(index: Int, possibles: List[Rule]) {
    def isResolved = possibles.size == 1

    def actual: (Int, Rule) = index -> possibles.head
  }


  case class Rule(ruleName: String, ranges: List[Range]) {
    def isValid(field: Int) = ranges.exists(r => r contains field)
  }

  private def getCandidateRules(state: State, validTickets: List[List[Int]]): List[Candidate] =
    validTickets.head.indices.toList
      .map { fieldIndex => validTickets.map(_ (fieldIndex)) }
      .zipWithIndex.map { case (fields, i) => Candidate(i, findCandidatesForField(fields, state.rules)) }

  private def getValidTickets(state: State): List[List[Int]] =
    state.otherTickets.filter(t => findFieldsWhichCannotBeValid(t, state.rules).isEmpty)

  override val expectedPartOne: Option[Int] = Some(71)

  override val expectedPartTwo: Option[Long] = Some(1)

  override val testData: List[String] = List(
    "class: 1-3 or 5-7",
    "row: 6-11 or 33-44",
    "seat: 13-40 or 45-50",
    "",
    "your ticket:",
    "7,1,14",
    "",
    "nearby tickets:",
    "7,3,47",
    "40,4,50",
    "55,2,20",
    "38,6,12")

  override val testData2: Option[List[String]] = Some(List(
    "class: 0-1 or 4-19",
    "row: 0-5 or 8-19",
    "seat: 0-13 or 16-19",
    "",
    "your ticket:",
    "11,12,13",
    "",
    "nearby tickets:",
    "3,9,18",
    "15,1,5",
    "5,14,9"))
}
