package AOC_2020

import shared.{DayChallenge, Helpers}
import java.lang.Long.parseUnsignedLong


object DayFourteen extends DayChallenge[Long, Long] with Helpers {

  override def partOne(l: List[String]): Long = getMemoryMap(Map[Long, Long](), l, "", updateMemoryMap).values.sum

  private def getMemoryMap(inMem: Map[Long, Long], instructions: List[String], mask: String, updateMapFunction: (Map[Long, Long], Instruction, String) => Map[Long, Long]): Map[Long, Long] = {
    if (instructions.isEmpty) {
      inMem
    } else {
      instructions.head.take(3) match {
        case "mas" => getMemoryMap(inMem, instructions.tail, getMask(instructions.head), updateMapFunction)
        case "mem" =>
          val instruction = getInstruction(instructions.head)
          getMemoryMap(updateMapFunction(inMem, instruction, mask), instructions.tail, mask, updateMapFunction)
        case _ => throw new RuntimeException("Didn't understand that")
      }
    }
  }

  private def updateMemoryMap(m: Map[Long, Long], instruction: Instruction, mask: String): Map[Long, Long] =
    m + (instruction.addressIndex -> getNewNumber(mask, instruction.number))

  private def applyMask(mask: String, i: Long): String =
    mask
      .zipWithIndex
      .filter { case (char, _) => char.isDigit }
      .foldLeft(getPaddedBinary(i)){ case (currentBinary, (binaryChar, index)) =>
        currentBinary.updated(index, binaryChar)
      }


  private def getNewNumber(mask: String, i: Long): Long = parseUnsignedLong(applyMask(mask, i), 2)

  private def getPaddedBinary(i: Long, len: Int = 36): String = "0" * (len - i.toBinaryString.length) + i.toBinaryString

  case class Instruction(addressIndex: Long, number: Long)

  private def getInstruction(string: String) = {
    val (addressString, intString) = getTwoFromSplit(string, " = ")
    Instruction(addressString.filter(_.isDigit).mkString.toLong, intString.toLong)
  }

  private def getMask(string: String) = getTwoFromSplit(string, "mask = ")._2

  override def partTwo(l: List[String]): Long = getMemoryMap(Map(), l, "", updateMapAtAddresses).values.sum

  private def updateMapAtAddresses(map: Map[Long, Long], instruction: Instruction, mask: String) =
    getNewAddresses(mask, instruction.addressIndex)
      .foldLeft(map)((newMap, address) => newMap + (address -> instruction.number))

  private def getNewAddresses(mask: String, address: Long): List[Long] =
    getReplacementChars(mask.count(_ == 'X'))
      .map(replacementChars =>
        applyMaskWithReplacements(mask, getPaddedBinary(address), replacementChars, 0, "")
      ).map(parseUnsignedLong(_, 2))

  private def applyMaskWithReplacements(mask: String, address: String, replacementChars: List[Char], i: Int, s: String): String =
    mask.foldLeft((s, replacementChars)){ case ((currentS, replacements), char) =>
      char match {
        case '0' => (currentS + char, replacements)
        case '1' => (currentS + '1', replacements)
        case 'X' =>
          replacements match {
            case head :: Nil => (currentS + head, List())
            case head :: tail => (currentS + head, tail)
            case _ => throw new RuntimeException("ran out of chars")
          }
        case _ => throw new RuntimeException("didn't understand that character")
      }
    }._1


  private def getReplacementChars(len: Int): List[List[Char]] =
    (0 until math.pow(2, len).toInt).map(getPaddedBinary(_, len).toCharArray.toList).toList

  override val expectedPartOne: Option[Long] = Some(165)
  override val expectedPartTwo: Option[Long] = Some(208)
  override val testData: List[String] = List("mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
    "mem[8] = 11",
    "mem[7] = 101",
    "mem[8] = 0")
  override val testData2: Option[List[String]] = Some(List("mask = 000000000000000000000000000000X1001X",
    "mem[42] = 100",
    "mask = 00000000000000000000000000000000X0XX",
    "mem[26] = 1"))


}
