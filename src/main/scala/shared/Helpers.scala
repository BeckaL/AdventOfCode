package shared

trait Helpers {
  def splitIntoGroupsOfString(l: List[String], currentGroup: String = "", groups: List[String] = List.empty): List[String] =
    l match {
      case List() => groups :+ currentGroup
      case head :: tail => head match {
          case ""             => splitIntoGroupsOfString(tail, "", groups :+ currentGroup)
          case individual @ _ => splitIntoGroupsOfString(tail, currentGroup + s" $individual", groups)
        }
    }

  def splitIntoGroupsOfList(
    l: List[String],
    currentGroup: List[String] = List(),
    groups: List[List[String]] = List.empty
  ): List[List[String]] =
    l match {
      case List() => groups :+ currentGroup
      case head :: tail => head match {
          case ""             => splitIntoGroupsOfList(tail, List(), groups :+ currentGroup)
          case individual @ _ => splitIntoGroupsOfList(tail, currentGroup :+ individual, groups)
        }
    }

  def getTwoFromSplit(str: String, split: String): (String, String) = {
    str.split(split) match {
      case Array(s1, s2: String) => (s1, s2)
      case a @ _                 => throw new RuntimeException(s"Should be an array of two things, was $a")
    }
  }

  def interpretNumberWithSign(str: String): Int =
    str.head match {
      case '+' => str.tail.mkString.toInt
      case '-' => 0 - str.tail.mkString.toInt
    }

  def interpretNumberWithOptionalSign(str: String): Int =
    str.head match {
      case '+' => str.tail.mkString.toInt
      case '-' => 0 - str.tail.mkString.toInt
      case _   => str.toInt
    }

  def indicesOf(string: String, char: Char): List[Int] =
    string.zipWithIndex.foldLeft(List[Int]()) { case (indices, (stringChar, i)) => if (stringChar == char) indices :+ i else indices }

  def indicesOf(l: List[String], str: String): List[Int] =
    l.zipWithIndex.foldLeft(List[Int]()) { case (indices, (stringChar, i)) => if (stringChar == str) indices :+ i else indices }

  def cycle(currentI: Int, len: Int): Int = (currentI + 1) % len

  def cycle[A](l: List[A], element: A): A = l(cycle(l.indexOf(element), l.size))

  def cycle[A](l: List[A], from: A, steps: Int): A = {
    val fromIndex = l.indexOf(from)
    l((fromIndex + steps) % l.size)
  }

  def rangeIncreasingOrDecreasing(fromN: Int, toN: Int) =
    if (fromN < toN) (fromN to toN).toList else (toN to fromN).reverse.toList
}
