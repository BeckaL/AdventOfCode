package AOC_2017

import shared.Primes.notPrime
import shared.{DayChallenge, TestData}

import scala.annotation.tailrec

object DayTwentyThree extends DayChallenge[Int, Int] {
  override def partOne(l: List[String]): Int = findNoOfTimesMulCalled(l)

  private def findNoOfTimesMulCalled(instructions: List[String]): Int = {
    @tailrec
    def go(i: Int, registers: Map[String, Int], mulCalled: Int): Int =
      if (!instructions.indices.contains(i))
        mulCalled
      else
        instructions(i).split(" ").toList match {
          case "set" :: x :: y :: Nil => go(i + 1, registers.set(x, y), mulCalled)
          case "sub" :: x :: y :: Nil => go(i + 1, registers.sub(x, y), mulCalled)
          case "mul" :: x :: y :: Nil => go(i + 1, registers.mul(x, y), mulCalled + 1)
          case "jnz" :: x :: y :: Nil => go(i + registers.newOffset(x, y), registers, mulCalled)
          case _ => throw new RuntimeException("No match")
        }

    go(0, Map("a" -> 0, "b" -> 0, "c" -> 0, "d" -> 0, "e" -> 0, "f" -> 0, "g" -> 0, "h" -> 0), 0)
  }

  override def partTwo(l: List[String]): Int =
    (0 to 1000).foldLeft(0) { case (counter, i) => if (notPrime(108400 + (17 * i))) counter + 1 else counter }

  implicit class RegistersOp(registers: Map[String, Int]) {
    def getOrInt(str: String) = registers.getOrElse(str, str.toInt)
    def mul(x: String, y: String): Map[String, Int] = registers.updated(x, registers(x) * getOrInt(y))
    def sub(x: String, y: String): Map[String, Int] = registers.updated(x, registers(x) - getOrInt(y))
    def set(x: String, y: String): Map[String, Int] = registers.updated(x, getOrInt(y))
    def newOffset(x: String, y: String): Int = if(registers.getOrInt(x) == 0) 1 else registers.getOrInt(y)
  }
}

object DayTwentyThreeData extends TestData[Int, Int] {
  override val testData: List[String] = List.empty
  //correct answer to real 6724
  override val expectedPartOne: Option[Int] = None
  //correct answer to real 903
  override val expectedPartTwo: Option[Int] = None
}
