package AOC_2020

import shared.{DayChallenge, Helpers}

object DayNineteen extends DayChallenge[Long, Long] with Helpers {
  override val expectedPartOne: Option[Long] = Some(1)
  override val expectedPartTwo: Option[Long] = Some(0)
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

  override def partOne(l: List[String]): Long = {
    val (rStrings, stringsWithEmptyHead) = l.splitAt(l.indexOf(""))
    val rules = getRules(rStrings)
//    rules.foreach { r =>
//      println(s"r index is ${r.i}")
//      println(s"r replacement string is ${r.replacementStr}")
//    }
    val reduced = reduceRules(rules, List())
    println(reduced.replacementStr)
    val segments = getSegments(reduced.asStr, List())
    println(segments.mkString("\n"))
    println(s"segments size is ${segments.size}")
    println(s"here 1")
    println(segments.map(injectAnds(_)).mkString("\n"))
    println(s"here 2")

    1
  }

  private def injectAnds(segment: String, i: Int = 0, currentSegment: String = ""): String = {
    if (i == segment.indices.end - 1) {
      currentSegment + segment(i)
    } else {
      val newStr = if (segment(i) == ')' && segment(i + 1) == '(') ")&" else segment(i)
      injectAnds(segment, i + 1, currentSegment + newStr)
    }
  }

  private def getSegments(str: String, segments: List[String]): List[String] = {
    if (str.contains('(')) {
      val firstOpening = str.indexOf('(')
      val segmentEnd = if (firstOpening != 0) {
        firstOpening - 1
      } else {
       findClosingBracket(str, 0, 0)
      }
      if (segmentEnd == str.indices.end) {
        segments :+ str
      } else {
        val (segment, remaining) = str.splitAt(segmentEnd + 1)
        getSegments(remaining, segments :+ segment)
      }
    } else {
      segments :+ str
    }
  }

  private def findClosingBracket(str: String, unclosedOpenings: Int, i: Int): Int = {
    val char = str(i)
    char match {
      case '(' => findClosingBracket(str, unclosedOpenings + 1, i + 1)
      case ')' =>
        unclosedOpenings - 1 match {
          case newUnclosed if newUnclosed == 0 => i
          case newUnclosed => findClosingBracket(str, newUnclosed, i + 1)
        }
      case _ => findClosingBracket(str, unclosedOpenings, i + 1)
    }
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


  override def partTwo(l: List[String]): Long = ???
}
