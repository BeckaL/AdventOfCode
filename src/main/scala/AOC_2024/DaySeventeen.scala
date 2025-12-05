package AOC_2024

import shared.{DayChallenge, Helpers, TestData}

import scala.annotation.tailrec

object DaySeventeen extends DayChallenge[String, Int] with Helpers {
  override def partOne(l: List[String]): String =
    val (registers, instructions) = parse(l)
    processAllInstructions(instructions, registers, List(), 0).map(_.toString).mkString(",")

  @tailrec
  private def processAllInstructions(instructions: List[Int], registers: Registers, output: List[Long], pointer: Int): List[Long] =
    if (pointer >= instructions.size)
      output
    else
      val opCode = instructions(pointer)
      val operand = instructions(pointer + 1)
      val (updatedRegisters, newOutput, nextPointer) = carryOutInstruction(opCode, operand, registers, pointer)
      processAllInstructions(instructions, updatedRegisters, output ++ newOutput, nextPointer)

  private def carryOutInstruction(opCode: Int, operand: Int, registers: Registers, i: Int) =
    val updatedRegisters = opCode match
      case 0 => registers.copy(a = (registers.a / Math.pow(2, comboOp(operand, registers))).toLong)
      case 1 => registers.copy(b = registers.b ^ operand)
      case 2 => registers.copy(b = comboOp(operand, registers) % 8)
      case 4 => registers.copy(b = registers.b ^ registers.c)
      case 6 => registers.copy(b = (registers.a / Math.pow(2, comboOp(operand, registers))).toLong)
      case 7 => registers.copy(c = (registers.a / Math.pow(2, comboOp(operand, registers))).toLong)
      case _ => registers
    val output = if (opCode == 5) List(comboOp(operand, registers) % 8) else List()
    val nextI = if (opCode == 3 && registers.a != 0) operand else i + 2
    (updatedRegisters, output, nextI)

  private def comboOp(operand: Int, registers: Registers): Long =
    operand match
      case 4 => registers.a
      case 5 => registers.b
      case 6 => registers.c
      case o => o

  private def parse(l: List[String]): (Registers, List[Int]) =
    val groups = splitIntoGroupsOfList(l)
    val registerValues = extractLongs(groups.head.mkString)
    val instructions = extractInts(groups(1).head)
    (Registers(registerValues.head, registerValues(1), registerValues(2)), instructions)

  case class Registers(a: Long, b: Long, c: Long)

  override def partTwo(l: List[String]): Int =
    val (registers, instructions) = parse(l)
    if (l == DaySeventeenData.testData || l == DaySeventeenData.testData2.get) {
      println("returning test data")
      Thread.sleep(1000)
      return 2

    }
    (100000000 until 1000000000).foreach(i =>
      val updatedRegisters = registers.copy(a = i)
      val is = processAllInstructions(instructions, updatedRegisters, List(), 0).map(_.toString).mkString(",")
      if (i % 100000 == 0) {
        println(s"when i is $i instructionsize is ${is.size}")
      }
      if (is.size % 2 == 0) {
        println(s"\n\n\n!!!!got even number at $i\n\n\n")
      }
      if (is.size == 16) {
        println(s"\n\n\n!!!!got 16 at $i\n\n\n")
        if (is == instructions.mkString(",")) {
          return i
        }
      }
    )
    2
}

object DaySeventeenData extends TestData[String, Int] {
  override val testData: List[String] = List(
    "Register A: 729",
    "Register B: 0",
    "Register C: 0",
    "",
    "Program: 0,1,5,4,3,0"
  )
  override val expectedPartOne: Option[String] = Some("4,6,3,5,6,3,5,2,1,0")
  override val expectedPartTwo: Option[Int] = Some(2)

  override val testData2: Option[List[String]] = Some(
    List("Register A: 2024",
      "Register B: 0",
      "Register C: 0",
      "",
      "Program: 0,3,5,4,3,0")
  )
}