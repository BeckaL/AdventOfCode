package AOC_2020

import shared.DayChallenge

object DayTwo extends DayChallenge[Int, Int] {
  def partOne(input: List[String]): Int =
    rulesAndPasswords(input).count { case (r, p) => r.i1 to r.i2 contains p.count(_ == r.char) }

  def partTwo(input: List[String]): Int =
    rulesAndPasswords(input).count { case (r, p) => exactlyOneEquals(r, p) }

  private def exactlyOneEquals(r: Rule, p: String) =
    p.zipWithIndex.count { case (c, i) => (i == r.i1 - 1 || i == r.i2 - 1) && c == r.char } == 1

  private def rulesAndPasswords(input: List[String]): List[(Rule, String)] =
    input.map(
      _.split(" ").toList match {
        case List(r, character, password) => (getRule(r, character), password)
        case _ => throw new RuntimeException("input invalid")
      }
    )

  private def getRule(r: String, character: String): Rule =
    r.split("-").toList match {
      case List(i1, i2) =>
        Rule(character.replace(":", "").toCharArray.head, i1.toInt, i2.toInt)
      case _ => throw new RuntimeException("Range invalid")
    }

  case class Rule(char: Char, i1: Int, i2: Int)

}
