package AOC_2020

import shared.{DayChallenge, Helpers}

import scala.collection.Set
import scala.util.Try

object DaySeven extends DayChallenge[Int, Int] with Helpers {
  private val shinyGoldBags = "shiny gold bags"

  override def partOne(l: List[String]): Int =
    getAllContaining(shinyGoldBags, getAllBags(l), Set()).size

  private def getAllContaining(str: String, allBags: List[BagWithContents], known: Set[String]): Set[String] =
    allBags.collect { case b if b.contents.map(_.bagName) contains str => b.bagName }.toSet match {
      case x if x.isEmpty => known
      case newBags@_ => newBags.flatMap(b => getAllContaining(b, allBags, known | newBags))
    }

  override def partTwo(l: List[String]): Int =
    getBagTree(shinyGoldBags, 1, getAllBags(l)).getValue - 1

  private def getAllBags(l: List[String]): List[BagWithContents] =
    l.map(line => getTwoFromSplit(line, " contain ")).map {
      case (bag, contents) => BagWithContents(
        bag,
        contents.split(", ").map { cs => BagWithInt.from(normalizeStringWithDigit(cs)) }.toList
      )
    }

  private def getBagTree(bagName: String, n: Int, l: List[BagWithContents]): BagTree =
    l.find(_.bagName == bagName) match {
      case None => BagTree(n, List())
      case Some(bagWithContents) => BagTree(n, bagWithContents.contents.map(c => getBagTree(c.bagName, c.i, l)))
    }

  private def normalizeStringWithDigit(str: String): (Int, String) =
    str.replace(".", "").filter(!_.isDigit).trim() match {
      case s if s.last == 's' =>
        (Try(str.filter(_.isDigit).mkString("").toInt).toOption.getOrElse(0), s)
      case string@_ => (Try(str.filter(_.isDigit).mkString("").toInt).toOption.getOrElse(0), string + 's')
    }

  override val testData: List[String] = List(
    "light red bags contain 1 bright white bag, 2 muted yellow bags.",
    "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
    "bright white bags contain 1 shiny gold bag.",
    "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
    "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
    "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
    "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
    "faded blue bags contain no other bags.",
    "dotted black bags contain no other bags.",
  )

  override val expectedPartOne: Option[Int] = Some(4)

  override val expectedPartTwo: Option[Int] = Some(32)

}

case class BagWithInt(bagName: String, i: Int)

object BagWithInt {
  def from(input: (Int, String)) = BagWithInt(input._2, input._1)
}

case class BagWithContents(bagName: String, contents: List[BagWithInt])

case class BagTree(i: Int, contents: List[BagTree]) {
  val isLeaf: Boolean = contents.forall(_.i == 0)

  def getValue: Int = if (isLeaf) i else i + i * contents.map(_.getValue).sum
}