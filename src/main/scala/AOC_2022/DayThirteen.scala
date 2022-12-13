package AOC_2022

import shared.{DayChallenge, Helpers, TestData}

object DayThirteen extends DayChallenge[Int, Int] with Helpers {
  override def partOne(l: List[String]): Int =
    splitIntoGroupsOfList(l).zipWithIndex.collect { case (pairs, i)
      if leftIsSmallerThanRight(parse(pairs(0)), parse(pairs(1))).get => i + 1
    }.sum

  override def partTwo(l: List[String]): Int = {
    val (separatorOne, separatorTwo) = (parse("[[2]]"), parse("[[6]]"))
    val sorted = (l.filterNot(_ == "").map(parse) ++ List(separatorOne, separatorTwo)).sorted(TreeOrdering)
    (sorted.indexOf(separatorOne) + 1) * (sorted.indexOf(separatorTwo) + 1)
  }

  private def leftIsSmallerThanRight(left: Tree, right: Tree): Option[Boolean] =
    (left, right) match {
      case (Number(l), Number(r)) => if (l < r) Some(true) else if (r < l) Some(false) else None
      case (leftNumber: Number, _) => leftIsSmallerThanRight(TreeList(List(leftNumber)), right)
      case (_, rightNumber: Number) => leftIsSmallerThanRight(left, TreeList(List(rightNumber)))
      case (TreeList(leftItems), TreeList(rightItems)) => compareRightAndLeftLists(leftItems, rightItems)
    }

  private def compareRightAndLeftLists(left: List[Tree], right: List[Tree]): Option[Boolean] =
    (left, right) match {
      case (Nil, Nil) => None
      case (Nil, _) => Some(true)
      case (_, Nil) => Some(false)
      case (leftHead :: leftTail, rightHead :: rightTail) => leftIsSmallerThanRight(leftHead, rightHead) match {
        case None => compareRightAndLeftLists(leftTail, rightTail)
        case someResult@_ => someResult
      }
    }

  private def parse(s: String): Tree =
    s.head match {
      case '[' =>
        val inner = s.tail.dropRight(1)
        if (inner.isEmpty) TreeList(List()) else TreeList(splitAtTopLevelCommas(inner).map(parse))
      case _ => Number(v = s.toInt)
    }

  private def splitAtTopLevelCommas(s: String): List[String] = {
    val indices = indicesOfTopLevelCommas(s)
    val ends = indices :+ s.size
    val starts = 0 +: indices.map(_ + 1)
    starts.zip(ends).foldLeft(List[String]()) { case (itemsSoFar, (from, to)) => itemsSoFar :+ s.slice(from, to) }
  }

  private def indicesOfTopLevelCommas(s: String, found: List[Int] = List(), nestingLevel: Int = 0, i: Int = 0): List[Int] =
    s(i) match {
      case '[' => indicesOfTopLevelCommas(s, found, nestingLevel + 1, i + 1)
      case ']' => if (i == s.indices.last) found else indicesOfTopLevelCommas(s, found, nestingLevel - 1, i + 1)
      case ',' if nestingLevel == 0 => indicesOfTopLevelCommas(s, found :+ i, nestingLevel, i + 1)
      case _ => if (i == s.indices.last) found else indicesOfTopLevelCommas(s, found, nestingLevel, i + 1)
    }

  trait Tree

  case class Number(v: Int) extends Tree

  case class TreeList(items: List[Tree]) extends Tree

  object TreeOrdering extends Ordering[Tree] {
    def compare(l: Tree, r: Tree): Int = leftIsSmallerThanRight(l, r) match {
      case Some(true) => -1
      case Some(false) => 1
      case None => 0
    }
  }
}

object DayThirteenData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "[1,1,3,1,1]",
    "[1,1,5,1,1]",
    "",
    "[[1],[2,3,4]]",
    "[[1],4]",
    "",
    "[9]",
    "[[8,7,6]]",
    "",
    "[[4,4],4,4]",
    "[[4,4],4,4,4]",
    "",
    "[7,7,7,7]",
    "[7,7,7]",
    "",
    "[]",
    "[3]",
    "",
    "[[[]]]",
    "[[]]",
    "",
    "[1,[2,[3,[4,[5,6,7]]]],8,9]",
    "[1,[2,[3,[4,[5,6,0]]]],8,9]"
  )
  override val expectedPartOne: Option[Int] = Some(13)
  override val expectedPartTwo: Option[Int] = Some(140)
}