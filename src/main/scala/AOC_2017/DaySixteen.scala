package AOC_2017

import shared.{DayChallenge, Helpers}

object DaySixteen extends DayChallenge[String, String] with Helpers {
  override def partOne(l: List[String]): String =
    dance(startingConfiguration(l), getInstructions(l)).mkString

  override def partTwo(l: List[String]): String = {
    val allConfigs = danceUntilRepeat(getInstructions(l), List(startingConfiguration(l)))
    val cycleSpan = allConfigs.size
    allConfigs((targetIterations(l) % cycleSpan).toInt).mkString
  }

  private def dance(startingConfiguration: List[Char], instructions: List[Instruction]): List[Char] =
    instructions.foldLeft(startingConfiguration){
      case (currentConfiguration, instruction) => instruction.go(currentConfiguration)
    }

  private def danceUntilRepeat(instructions: List[Instruction], pastConfigs: List[List[Char]]): List[List[Char]] = {
    val newConfig = dance(pastConfigs.head, instructions)
    if (pastConfigs.contains(newConfig))
      pastConfigs.reverse
    else
      danceUntilRepeat(instructions, newConfig +: pastConfigs)
  }

  private def getInstructions(l: List[String]): List[Instruction] =
    l.head.split(",").toList
      .map { s =>
        s.head.toLower match {
          case 's' => Spin(s.tail.toInt)
          case 'x' =>
            val (positionA, positionB) = getTwoFromSplit(s.tail, "/")
            Exchange(positionA.toInt, positionB.toInt)
          case 'p' =>
            val (charsA, charsB) = getTwoFromSplit(s.tail.toLowerCase, "/")
            Partner(charsA.head, charsB.head)
          case _ => throw new RuntimeException(s"didn't understand instruction $s")
        }
      }

  trait Instruction { def go(current: List[Char]): List[Char]}
  case class Spin(n: Int) extends Instruction {
    def go(current: List[Char]): List[Char] = current.takeRight(n) ++ current.take(current.size - n)
  }
  case class Exchange(positionA: Int, positionB: Int) extends Instruction {
    def go(current: List[Char]): List[Char] =
      current.updated(positionA, current(positionB)).updated(positionB, current(positionA))
  }
  case class Partner(a: Char, b: Char) extends Instruction {
    override def go(current: List[Char]): List[Char] =
      current.updated(current.indexOf(a), b).updated(current.indexOf(b), a)
  }

  def startingConfiguration(l: List[String]): List[Char] =
    if (l == testData) "abcde".toCharArray.toList else "abcdefghijklmnop".toCharArray.toList

  def targetIterations(l: List[String]): Long = if (l == testData) 2 else 1000000000

  override val expectedPartOne: Option[String] = Some("baedc")
  override val testData: List[String] = List("s1,x3/4,pe/b")
  override val expectedPartTwo: Option[String] = Some("ceadb")
}
