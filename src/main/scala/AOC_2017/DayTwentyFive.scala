package AOC_2017

import shared.{DayChallenge, TestData}

object DayTwentyFive extends DayChallenge[Int, Int] {
  override def partOne(l: List[String]): Int = {
    val (startState, numberOfSteps, instructions) = toStateInstructions(l)
    carryOutInstructions(startState, numberOfSteps, instructions).values.sum
  }

  def carryOutInstructions(startState: String, numberOfSteps: Int, instructions: Map[String, StateInstruction]): Map[Int, Int] = {
    def go(state: String, currentStepsDone: Int, tape: Map[Int, Int], currentPosition: Int): Map[Int, Int] = {
      if (currentStepsDone == numberOfSteps)
        tape
      else {
        val stateInstruction = instructions(state)
        val instruction = if (tape.getOrElse(currentPosition, 0) == 0) stateInstruction.if0 else stateInstruction.if1
        val newTape = tape.updated(currentPosition, instruction.writeValue)
        val newCurrentPosition = if (instruction.moveDirection == MoveDirection.Right) currentPosition + 1 else currentPosition - 1
        go(instruction.nextStateInstruction, currentStepsDone + 1, newTape, newCurrentPosition)
      }
    }
    go(startState, 0, Map(), 0)
  }

  override def partTwo(l: List[String]): Int = 1 //not needed

  def toStateInstructions(l: List[String]): (String, Int, Map[String, StateInstruction]) = {
    val startState = l.head.replaceAll("\\.", "").last.toString

    val numberOfSteps = l(1).split(" ")(5).toInt

    val remainingText = l.slice(3, l.size)
    (startState, numberOfSteps, remainingText.grouped(10).toList.map(StateInstruction.from).toMap)
  }


  case class StateInstruction(if0: StateBranch, if1: StateBranch)

  object StateInstruction {
    def from(lines: List[String]): (String, StateInstruction) = {
      val if0 = StateBranch.from(lines.slice(2, 5))
      val if1 = StateBranch.from(lines.slice(6, 9))
      (lines.head(9).toString, StateInstruction(if0, if1))
    }
  }


  trait MoveDirection

  object MoveDirection {

    case object Right extends MoveDirection

    case object Left extends MoveDirection

    def from(s: String) = s match {
      case "right" => Right
      case "left" => Left
      case _ => throw new RuntimeException(s"Didn't understand s ${s} as move direction left or right")
    }
  }

  case class StateBranch(writeValue: Int, moveDirection: MoveDirection, nextStateInstruction: String)

  object StateBranch {
    def from(ls: List[String]): StateBranch = {
      val writeValue = ls.head.find(char => char == '0' || char == '1').map(_.toString.toInt).get
      println(writeValue)
      val moveDirection = ls(1).replaceAll("\\.", "").split(" ")
        .find(word => word == "right" || word == "left").map(MoveDirection.from).get
      val nextStateInstruction = ls(2).replaceAll("\\.", "").split(" ").last
      StateBranch(writeValue, moveDirection, nextStateInstruction)
    }
  }

}
object DayTwentyFiveData extends TestData[Int, Int] {
  override val testData: List[String] = List(
      "Begin in state A.",
      "Perform a diagnostic checksum after 6 steps.",
      "      ",
      "In state A:",
      "  If the current value is 0:",
      "    - Write the value 1.",
      "    - Move one slot to the right.",
      "    - Continue with state B.",
      "  If the current value is 1:",
      "    - Write the value 0.",
      "    - Move one slot to the left.",
      "    - Continue with state B.",
      "      ",
      "In state B:",
      "  If the current value is 0:",
      "    - Write the value 1.",
      "    - Move one slot to the left.",
      "    - Continue with state A.",
      "  If the current value is 1:",
      "    - Write the value 1.",
      "    - Move one slot to the right.",
      "    - Continue with state A.")

  override val expectedPartOne: Option[Int] = Some(3)
  override val expectedPartTwo: Option[Int] = None
}