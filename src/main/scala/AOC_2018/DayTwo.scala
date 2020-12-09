package AOC_2018

import shared.DayChallenge

object DayTwo extends DayChallenge[Int, String] {
  override def partOne(l: List[String]): Int = l.count(containsLetterTwice) * l.count(containsLetterThrice)

  private def containsLetterTwice(string: String) = string.distinct.map(char => string.count(_ == char)).contains(2)
  private def containsLetterThrice(string: String) = string.distinct.map(char => string.count(_ == char)).contains(3)

  override def partTwo(l: List[String]): String = findWord(l.head, l.tail) match {
    case Some(w2) => l.head.intersect(w2).mkString
    case None => partTwo(l.tail)
  }

  private def findWord(w1: String, l: List[String]): Option[String] =
    l.find(w1.zip(_).count { case (c1, c2) => c1 == c2 } == w1.length - 1)
}
