package AOC_2017

import shared.{DayChallenge, TestData}

object DayNine extends DayChallenge[Int, Int] {

  override def partOne(l: List[String]): Int =
    l.map(_.removeIgnored.removeGarbage).map(countGroups).sum

  private def countGroups(s: String): Int =
      s.foldLeft((0, 0)){case ((unterminatedGroups, runningTotal), char) =>
        char match {
          case '}' => (unterminatedGroups - 1, unterminatedGroups + runningTotal)
          case '{' => (unterminatedGroups + 1, runningTotal)
          case _ => (unterminatedGroups, runningTotal)
        }
      }._2

  override def partTwo(l: List[String]): Int = l.map(_.removeIgnored).map(countCharactersInGarbage(_)).sum

  def countCharactersInGarbage(s: String, runningTotal: Int = 0): Int =
    s.indexOf('<') match {
      case -1 => runningTotal
      case index @ _ =>
        val until = s.takeRight(s.size - index).indexOf('>') + index
        countCharactersInGarbage(s.takeRight(s.size - 1 - until), runningTotal + (until - 1 - index))
    }

  implicit class StringOps (s: String) {
    def removeIgnored: String =
      s.indexOf('!') match {
        case -1 => s
        case index @ _ => (s.take(index) ++ s.takeRight(s.length - 2 - index)).removeIgnored
      }

    def removeGarbage: String =
      s.indexOf('<') match {
          case -1 => s
          case index @ _ =>
            val until = s.takeRight(s.size - 1 - index).indexOf('>') + index
            s.take(index) ++ s.takeRight(s.size - 2 - until).removeGarbage
        }
  }
}

object DayNineData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "{}",
    "{{{}}}",
    "{{},{}}",
    "{{{},{},{{}}}}",
    "{<a>,<a>,<a>,<a>}",
    "{{<ab>},{<ab>},{<ab>},{<ab>}}",
    "{{<!!>},{<!!>},{<!!>},{<!!>}}",
    "{{<a!>},{<a!>},{<a!>},{<ab>}}"
  )
  override val expectedPartOne: Option[Int] = Some(50)

  override val testData2: Option[List[String]] = Some(List(
    "<>",
    "<random characters>",
    "<<<<>",
    "<{!>}>",
    "<!!>",
    "<!!!>>",
    "<{o'i!a,<{i<a>"
  ))
  override val expectedPartTwo: Option[Int] = Some(32)
}