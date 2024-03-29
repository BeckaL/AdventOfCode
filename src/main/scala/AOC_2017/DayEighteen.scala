package AOC_2017

import shared.{DayChallenge, TestData}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object DayEighteen extends DayChallenge[Long, Int] {
  override def partOne(l: List[String]): Long =
    interpret(l, State(None, Map.empty[String, Long]), 0)


  case class State(lastSound: Option[Long], m: Map[String, Long]) {
    def get(x: String): Long = Try(x.toInt) match {
      case Success(i) => i
      case _ => m.getOrElse(x, 0)
    }

    def multiply(x: String, by: String) = State(lastSound, m.updated(x, get(x) * get(by)))

    def increase(x: String, by: String): State = State(lastSound, m.updated(x, get(x) + get(by)))

    def modulo(x: String, by: String): State = State(lastSound, m.updated(x, get(x) % get(by)))

    def set(x: String, to: String): State = State(lastSound, m.updated(x, get(to)))

    def sound(x: String): State = State(Some(get(x)), m)

    def offset(x: String, y: String): Long = if (get(x) > 0) get(y) else 1
  }


  def interpret(instructions: List[String], s: State, currentI: Int): Long = {
    instructions(currentI).split(" ").toList match {
      case "snd" :: x :: Nil =>
        interpret(instructions, s.sound(x), currentI + 1)
      case "set" :: x :: by :: Nil =>
        interpret(instructions, s.set(x, by), currentI + 1)
      case "add" :: x :: by :: Nil =>
        interpret(instructions, s.increase(x, by), currentI + 1)
      case "mul" :: x :: by :: Nil =>
        interpret(instructions, s.multiply(x, by), currentI + 1)
      case "mod" :: x :: by :: Nil =>
        interpret(instructions, s.modulo(x, by), currentI + 1)
      case "rcv" :: x :: Nil =>
        if (s.m(x) == 0) interpret(instructions, s, currentI + 1) else s.lastSound.get
      case "jgz" :: x :: y :: Nil =>
        interpret(instructions, s, currentI + s.offset(x, y).toInt)
      case str@_ => throw new RuntimeException(s"Didn't understand instruction $str")
    }
  }

  override def partTwo(l: List[String]): Int = {
    val p0 = ParallelProgram(0, 0, List.empty, 0, Map("p" -> 0), terminated = false)
    val p1 = ParallelProgram(1, 0, List.empty, 0, Map("p" -> 1), terminated = false)
    parallelInterpret(l, p0, p1).find(_.id == 1).get.noSent
  }

  case class ParallelProgram(id: Int, position: Int, queue: List[Long], noSent: Int, values: Map[String, Long], terminated: Boolean) {
    def multiply(x: String, by: String) = this.copy(values = values.updated(x, get(x) * get(by))).setStateToRunning.incrementPositionBy1

    def increase(x: String, by: String): ParallelProgram = this.copy(values = values.updated(x, get(x) + get(by))).setStateToRunning.incrementPositionBy1

    def modulo(x: String, by: String): ParallelProgram = this.copy(values = values.updated(x, get(x) % get(by))).setStateToRunning.incrementPositionBy1

    def set(x: String, to: String): ParallelProgram = this.copy(values = values.updated(x, get(to))).setStateToRunning.incrementPositionBy1

    def send(x: String): (Long, ParallelProgram) = (get(x), this.copy(noSent = noSent + 1).setStateToRunning.incrementPositionBy1)

    def offset(x: String, y: String): ParallelProgram = this.copy(position = position + (if (get(x) > 0) get(y) else 1).toInt).setStateToRunning

    def receive(storeAt: String) = queue match {
      case Nil => this.copy(terminated = true)
      case valueToStore :: others => this.copy(queue = others, values = values.updated(storeAt, valueToStore)).setStateToRunning.incrementPositionBy1
    }

    def addToQueue(x: Long) = this.copy(queue = queue :+ x)

    private def setStateToRunning = this.copy(terminated = false)

    private def incrementPositionBy1 = this.copy(position = position + 1)

    private def get(x: String): Long = Try(x.toInt) match {
      case Success(i) => i
      case _ => values.getOrElse(x, 0)
    }
  }

  @tailrec
  final def parallelInterpret(instructions: List[String], active: ParallelProgram, inactive: ParallelProgram): List[ParallelProgram] = {
    if (active.terminated && inactive.terminated) {
      List(active, inactive)
    } else {
      Try(instructions(active.position)) match {
        case Failure(_) =>
          parallelInterpret(instructions, inactive, active.copy(terminated = true))
        case Success(instruction) =>
          instruction.split(" ").toList match {
            case "snd" :: x :: Nil =>
              val (sent, newActive) = active.send(x)
              parallelInterpret(instructions, inactive.addToQueue(sent), newActive)
            case "set" :: x :: to :: Nil =>
              parallelInterpret(instructions, inactive, active.set(x, to))
            case "add" :: x :: by :: Nil =>
              parallelInterpret(instructions, inactive, active.increase(x, by))
            case "mul" :: x :: by :: Nil =>
              parallelInterpret(instructions, inactive, active.multiply(x, by))
            case "mod" :: x :: by :: Nil =>
              parallelInterpret(instructions, inactive, active.modulo(x, by))
            case "rcv" :: x :: Nil =>
              parallelInterpret(instructions, inactive, active.receive(x))
            case "jgz" :: x :: by :: Nil =>
              parallelInterpret(instructions, inactive, active.offset(x, by))
            case str@_ => throw new RuntimeException(s"Didn't understand instruction $str")
          }
      }
    }
  }
}
object DayEighteenData extends TestData[Long, Int] {
  override val expectedPartOne: Option[Long] = Some(4)
  override val testData: List[String] = List(
    "set a 1",
    "add a 2",
    "mul a a",
    "mod a 5",
    "snd a",
    "set a 0",
    "rcv a",
    "jgz a -1",
    "set a 1",
    "jgz a -2"
  )
  override val expectedPartTwo: Option[Int] = Some(3)
  override val testData2: Option[List[String]] = Some(List(
    "snd 1",
    "snd 2",
    "snd p",
    "rcv a",
    "rcv b",
    "rcv c",
    "rcv d"
  ))
}