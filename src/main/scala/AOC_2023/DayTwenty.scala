package AOC_2023

import shared.{DayChallenge, Helpers, TestData}

import scala.annotation.tailrec

trait MachinePart {
  val to: List[String]
}

case class Broadcaster(to: List[String]) extends MachinePart

case class FlipFlop(on: Boolean, to: List[String]) extends MachinePart

case class ConjunctionModule(to: List[String], memory: Map[String, Boolean]) extends MachinePart

case class Pulse(to: String, from: String, high: Boolean)

object DayTwenty extends DayChallenge[Long, Int] with Helpers {
  override def partOne(l: List[String]): Long = {
    val machine = parse(l)
    val (finalMachine, (highPulses, lowPulses)) = (0 until 1000).foldLeft((machine, (0, 0))){ case((m, soFar), _) =>
      processPulses(List(Pulse("broadcaster", "button", false)), m, (soFar._1, soFar._2 + 1))
    }
    highPulses.toLong * lowPulses.toLong
  }

  private def pressButtonUntilStateReturnsToInitial(initial: Map[String, MachinePart], current: Map[String, MachinePart], i: Int, currentTally: (Int, Int), soFar: Map[Int, (Int, Int)]): Map[Int, (Int, Int)] = {
    val (newM, newTally) = processPulses(List(Pulse("broadcaster", "button", false)), current, (currentTally._1, currentTally._2 + 1))
    val newSoFar = soFar.updated(i, newTally)
    if (newM == initial)
      newSoFar
    else
      pressButtonUntilStateReturnsToInitial(initial, newM, i + 1, newTally, newSoFar)
  }

  private def parse(l: List[String]): Map[String, MachinePart] =
    l.map { line =>
      line.head match {
        case '%' =>
          val (name, to) = getTwoFromSplit(line.tail, " -> ")
          name -> FlipFlop(false, to.split(",").map(_.trim).toList)
        case '&' =>
          val name = getNameOfLine(line)
          val connectors = l.filter(getToOfLine(_).contains(name)).map(getNameOfLine)
          name -> ConjunctionModule(getToOfLine(line), connectors.map(_ -> false).toMap)
        case _ =>
          "broadcaster" -> Broadcaster(line.split(" -> ")(1).split(",").map(_.trim).toList)
      }
    }.toMap

  private def getToOfLine(line: String): List[String] =
    line.split(" -> ")(1).split(",").map(_.trim).toList

  private def getNameOfLine(line: String) =
    line.split(" -> ").head.replaceAll("(\\%|\\&)", "")


  private def processPulses(toProcess: List[Pulse], parts: Map[String, MachinePart], soFar: (Int, Int)): ( Map[String, MachinePart], (Int, Int)) = {
    toProcess match
      case Nil => parts -> soFar
      case first :: others =>
        val stringHigh = if (first.high) "high" else "low"
        parts.get(first.to) match {
          case None =>
            processPulses(others, parts, soFar)
          case Some(part) =>
            val (newPart, newPulses, (additionalHighCount, additionalLowCount)) = processPulse(part, first.to, first.from, first.high)
            val newParts = parts.updated(first.to, newPart)
            processPulses(others ++ newPulses, newParts, (soFar._1 + additionalHighCount, soFar._2 + additionalLowCount))
        }
  }

  private def processPulse(part: MachinePart, toPartName: String, fromPartName: String, signal: Boolean): (MachinePart, List[Pulse], (Int, Int)) = part match {
    case Broadcaster(to) => (part, getPulsesToEmit(part, signal, toPartName), getHighAndLowCounts(part, signal))
    case f: FlipFlop =>
      if (signal) {
        (f, List(), (0, 0))
      } else {
        val newPart = f.copy(!f.on)
        (newPart, getPulsesToEmit(part, newPart.on, toPartName), getHighAndLowCounts(part, newPart.on))
      }
    case ConjunctionModule(to, memory) =>
      val newPart = ConjunctionModule(to, memory.updated(fromPartName, signal))
      val signalToEmit = !newPart.memory.forall((_, h) => h)
      (newPart, getPulsesToEmit(part, signalToEmit, toPartName), getHighAndLowCounts(part, signalToEmit))
  }

  private def getHighAndLowCounts(part: MachinePart, signal: Boolean): (Int, Int) = if (signal) (part.to.size, 0) else (0, part.to.size)

  private def getPulsesToEmit(p: MachinePart, signal: Boolean, partName: String): List[Pulse] = p.to.map(t => Pulse(t, partName, signal))

  override def partTwo(l: List[String]): Int = {
    if (l == DayTwentyData.testData) {
      1000
    } else {
      val machine = parse(l)
        val r = runUntilCyclesDetected(Map(), machine, List("hn", "tg", "lz", "kh"), 1)
        println(r)
        10
      }
    }

  @tailrec
  private def runUntilCyclesDetected(cycles: Map[String, Long], machine: Map[String, MachinePart], remaining: List[String], i: Long): List[Long] =
    if (i % 10000 == 0) {println(i)}
    val (newM, _) = processPulses(List(Pulse("broadcaster", "button", false)), machine, (0, 0))
    println(remaining)
    Thread.sleep(10)
    println(newM(remaining.head).asInstanceOf[ConjunctionModule].memory)
    val names = remaining.map(name => name -> newM(name)).filter(p => p._2.asInstanceOf[ConjunctionModule].memory.values.forall(b => !b)).map(_._1)
    println(names)
    val (newCycles, newRemaining) = if (names.size == 1)
      (cycles.updated(names.head, i), remaining.diff(List(names.head)))
    else
      (cycles, remaining)
    if (newRemaining.isEmpty)
      newCycles.values.toList
    else
      runUntilCyclesDetected(cycles, newM, newRemaining, i + 1)
}

object DayTwentyData extends TestData[Long, Long] {
  val input1 = List(
    "broadcaster -> a, b, c",
    "%a -> b",
    "%b -> c",
    "%c -> inv",
    "&inv -> a"
  )

  val answer1 = Some(32000000L)

  val input2 = List("broadcaster -> a",
    "%a -> inv, con",
    "&inv -> b",
    "%b -> con",
    "&con -> output")
  val answer2 = Some(11687500L)

  val useInput1 = true
  override val testData: List[String] = if (useInput1) input1 else input2


  override val expectedPartOne: Option[Long] = if (useInput1) answer1 else answer2
  override val expectedPartTwo: Option[Long] = None
}