package AOC_2023

import shared.{DayChallenge, Helpers, TestData}

case class Part(x: Int, m: Int, a: Int, s: Int)
trait MachineFlow
case class Result(r: String) extends MachineFlow

trait Comparison extends MachineFlow {
  val partString: String;
  val comparitor: Int;
  val ifTrue: String
}
case class GreaterThanCondition(partString: String, comparitor: Int, ifTrue: String) extends Comparison
case class LessThanCondition(partString: String, comparitor: Int, ifTrue: String) extends Comparison

object DayNineteen extends DayChallenge[Int, Long] with Helpers {
  override def partOne(l: List[String]): Int =
    val (parts, machines) = parsePartsAndMachine(l)
    val validRanges = processFlow(machines("in"), Map(), machines, List())
    val validParts = parts.filter(p => validRanges.exists(r =>
      r.get("s").forall(_.contains(p.s)) &&
        r.get("x").forall(_.contains(p.x)) &&
        r.get("a").forall(_.contains(p.a)) &&
        r.get("m").forall(_.contains(p.m))))
    validParts.map(part => part.x + part.m + part.a + part.s).sum

  override def partTwo(l: List[String]): Long =
    val (_, machines) = parsePartsAndMachine(l)
    processFlow(machines("in"), Map(), machines, List()).map(calculatePossibilitiesInRanges).sum

  private def calculatePossibilitiesInRanges(ranges: Map[String, Range]): Long =
    val getRangeSizeForKey = (s: String) => ranges.get(s).map(range => range.size).getOrElse(4000).toLong
    getRangeSizeForKey("s") * getRangeSizeForKey("x") * getRangeSizeForKey("m") * getRangeSizeForKey("a")

  private def splitRange(r: Range, c: Comparison) = c match
    case _: GreaterThanCondition =>
      val (r1, r2) = r.splitAt(c.comparitor + 1 - r.start)
      (r2, r1)
    case _: LessThanCondition => r.splitAt(c.comparitor - r.start)

  private def processFlow(flow: List[MachineFlow], ranges: Map[String, Range], machine: Map[String, List[MachineFlow]], found: List[Map[String, Range]]): List[Map[String, Range]] =
    flow.head match
      case Result(str) if str == "A" => ranges +: found
      case Result(str) if str == "R" => found
      case Result(machinePartString) => processFlow(machine(machinePartString), ranges, machine, found)
      case c: Comparison =>
        val (trueRange, falseRange) = splitRange(ranges.getOrElse(c.partString, Range(1, 4001)), c)
        val ifTrueResult = c.ifTrue match
          case "A" => ranges.updated(c.partString, trueRange) +: found
          case "R" => found
          case nextPart => processFlow(machine(nextPart), ranges.updated(c.partString, trueRange), machine, found)
        ifTrueResult ++ processFlow(flow.tail, ranges.updated(c.partString, falseRange), machine, List())

  private def parsePartsAndMachine(l: List[String]) =
    val split = splitIntoGroupsOfList(l)
    val parts = split(1).map(extractInts).map(ints => Part(x = ints.head, m = ints(1), a = ints(2), s = ints(3)))
    (parts, split.head.map(parseMachine).toMap)

  private def parseMachine(s: String): (String, List[MachineFlow]) =
    val split = s.split("\\{")
    val flows = split(1).replaceAll("(\\{|\\})", "").split(",").map { conditionString =>
      if (conditionString.contains(":"))
        val (condition, result) = conditionString.splitAt(conditionString.indexOf(":"))
        val valueToCompare = extractInts(condition).head
        if (condition.contains("<"))
          LessThanCondition(condition.split("<").head, valueToCompare, result.tail)
        else
          GreaterThanCondition(condition.split(">").head, valueToCompare, result.tail)
      else
        Result(conditionString)
    }
    split.head -> flows.toList
}

object DayNineteenData extends TestData[Int, Long] {
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
  override val expectedPartTwo: Option[Long] = Some(167409079868000L)
}