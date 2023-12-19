package AOC_2023

import shared.{DayChallenge, Helpers, TestData}

object DayNineteen extends DayChallenge[Int, Int] with Helpers {
  override def partOne(l: List[String]): Int = 
    val split = splitIntoGroupsOfList(l)
    val parts = parseParts(split(1))
    val machines = split.head.map(parseMachine).toMap
    parts.filter(p => putPartThroughMachines(machines, p, "in")).map(part => part.x + part.m + part.a + part.s ).sum

  private def putPartThroughMachines(machines: Map[String, List[MachineFLow]], part: Part, current: String): Boolean =
    putPartThroughMachine(machines(current), part) match
      case "A" => true
      case "R" => false
      case differentMachine => putPartThroughMachines(machines, part, differentMachine)

  private def putPartThroughMachine(flows: List[MachineFLow], part: Part): String =
    flows.head match
      case Result(r) => r
      case Condition(f, ifTrue) => if (f(part)) ifTrue else putPartThroughMachine(flows.tail, part)

  case class Part(x: Int, m: Int, a: Int, s: Int)

  trait MachineFLow
  case class Condition(f: Part => Boolean, ifTrue: String) extends MachineFLow
  case class Result(r: String) extends MachineFLow

  private def parseParts(parts: List[String]) =
    parts.map(extractInts).map(ints => Part(x = ints.head, m = ints(1), a = ints(2), s = ints(3)))

  def getPart(s: String)(p: Part) = Map("x" -> p.x, "m" -> p.m, "a" -> p.a, "s" -> p.s)(s)

  private def parseMachine(s: String): (String, List[MachineFLow]) =
    val split = s.split("\\{")
    val flows = split(1).replaceAll("(\\{|\\})", "").split(",").map { conditionString =>
      if (conditionString.contains(":"))
        val (condition, result) = conditionString.splitAt(conditionString.indexOf(":"))
        val valueToCompare = extractInts(condition).head
        if (condition.contains("<"))
          val partFunction = getPart(condition.split("<").head)
          Condition((part: Part) => partFunction(part) < valueToCompare, result.tail)
        else
          val partFunction = getPart(condition.split(">").head)
          Condition((part: Part) => partFunction(part) > valueToCompare, result.tail)
      else
        Result(conditionString)
    }
    split.head -> flows.toList

  override def partTwo(l: List[String]): Int =
    2
}

object DayNineteenData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "px{a<2006:qkq,m>2090:A,rfg}",
    "pv{a>1716:R,A}",
    "lnx{m>1548:A,A}",
    "rfg{s<537:gd,x>2440:R,A}",
    "qs{s>3448:A,lnx}",
    "qkq{x<1416:A,crn}",
    "crn{x>2662:A,R}",
    "in{s<1351:px,qqz}",
    "qqz{s>2770:qs,m<1801:hdj,R}",
    "gd{a>3333:R,R}",
    "hdj{m>838:A,pv}",
    "",
    "{x=787,m=2655,a=1222,s=2876}",
    "{x=1679,m=44,a=2067,s=496}",
    "{x=2036,m=264,a=79,s=2244}",
    "{x=2461,m=1339,a=466,s=291}",
    "{x=2127,m=1623,a=2188,s=1013}"
  )
  override val expectedPartOne: Option[Int] = Some(19114)
  override val expectedPartTwo: Option[Int] = Some(0)
}