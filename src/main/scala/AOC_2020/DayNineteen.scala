package AOC_2020

import shared.{DayChallenge, Helpers}

object DayNineteen extends DayChallenge[Long, Long] with Helpers {


  override def partOne(l: List[String]): Long = {
    val (rStrings, stringsWithEmptyHead) = l.splitAt(l.indexOf(""))
    val rules = getRules(rStrings)
    println(rules.take(10))
    val reduced = reduceRules(getRules(rStrings), List())
    stringsWithEmptyHead.tail.count(candidateStr => candidateStr.matches(reduced.asStr))
  }

  private def reduceRules(rules: List[Rule], resolved: List[Int]): Rule = {
    if (rules.size == 1) {
      rules.head
    } else {
      val newResolved = rules.filter(_.isResolved).filterNot(resolved contains _)
      val replacementMap = newResolved.map { r => r.i -> r.replacementStr }
      val newRules = replacementMap.foldLeft(rules) { case (newRules, (index, replacement)) => newRules.map(_.replace(index, replacement)) }
      val newRulesWithoutResolve = newRules.filterNot(newResolved contains _)
      reduceRules(newRulesWithoutResolve, resolved ++ newResolved.map(_.i))
    }
  }

  private def getRuleI(rules: List[Rule], resolved: List[Int], i: Int): Rule = {
    val newResolved = rules.filter(_.isResolved).filterNot(resolved contains _)
    val replacementMap = newResolved.map { r => r.i -> r.replacementStr }
    val newRules = replacementMap.foldLeft(rules) { case (newRules, (index, replacement)) => newRules.map(_.replace(index, replacement)) }
    val ruleI = rules.find(_.i == i).get
    if (ruleI.isResolved) {
      ruleI
    } else {
      val newRulesWithoutResolve = newRules.filterNot(newResolved contains _).toList
      getRuleI(newRulesWithoutResolve, resolved ++ newResolved.map(_.i), i)
    }
  }

  private def getRules(rStrings: List[String]) = rStrings.map { r =>
    val split = r.split(": ")
    Rule(split(0).toInt, split(1).trim.split(" ").toList.map(_.replaceAll("\"", "")))
  }


  case class Rule(i: Int, strs: List[String]) {
    val isResolved = strs.forall(str => str.forall(!_.isDigit))
    val replacementStr = if (strs.length == 1) strs.head else s"(${strs.mkString})"
    val asStr = strs.mkString

    def replace(replacementI: Int, replacementString: String) = {
      val indicesToReplace = indicesOf(strs, replacementI.toString)
      val newStrings = indicesToReplace.foldLeft(strs)((newStrs, index) => newStrs.updated(index, replacementString))
      Rule(i, newStrings)
    }
  }




  override def partTwo(l: List[String]): Long = {
  }

  override val expectedPartOne: Option[Long] = Some(2)
  override val expectedPartTwo: Option[Long] = Some(12)
  override val testData2: Option[List[String]] = Some(List(
    "42: 9 14 | 10 1",
    "9: 14 27 | 1 26",
    "10: 23 14 | 28 1",
    "1: \"a\"",
    "11: 42 31",
    "5: 1 14 | 15 1",
    "19: 14 1 | 14 14",
    "12: 24 14 | 19 1",
    "16: 15 1 | 14 14",
    "31: 14 17 | 1 13",
    "6: 14 14 | 1 14",
    "2: 1 24 | 14 4",
    "0: 8 11",
    "13: 14 3 | 1 12",
    "15: 1 | 14",
    "17: 14 2 | 1 7",
    "23: 25 1 | 22 14",
    "28: 16 1",
    "4: 1 1",
    "20: 14 14 | 1 15",
    "3: 5 14 | 16 1",
    "27: 1 6 | 14 18",
    "14: \"b\"",
    "21: 14 1 | 1 14",
    "25: 1 1 | 1 14",
    "22: 14 14",
    "8: 42",
    "26: 14 22 | 1 20",
    "18: 15 15",
    "7: 14 5 | 1 21",
    "24: 14 1",
    "",
    "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa",
    "bbabbbbaabaabba",
    "babbbbaabbbbbabbbbbbaabaaabaaa",
    "aaabbbbbbaaaabaababaabababbabaaabbababababaaa",
    "bbbbbbbaaaabbbbaaabbabaaa",
    "bbbababbbbaaaaaaaabbababaaababaabab",
    "ababaaaaaabaaab",
    "ababaaaaabbbaba",
    "baabbaaaabbaaaababbaababb",
    "abbbbabbbbaaaababbbbbbaaaababb",
    "aaaaabbaabaaaaababaa",
    "aaaabbaaaabbaaa",
    "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa",
    "babaaabbbaaabaababbaabababaaab",
    "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"))
  override val testData: List[String] = List(
    "0: 4 1 5",
    "1: 2 3 | 3 2",
    "2: 4 4 | 5 5",
    "3: 4 5 | 5 4",
    "4: \"a\"",
    "5: \"b\"",
    "",
    "ababbb",
    "bababa",
    "abbbab",
    "aaabbb",
    "aaaabbb",
  )
}
