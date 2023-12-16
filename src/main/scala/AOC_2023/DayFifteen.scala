package AOC_2023

import shared.{DayChallenge, Helpers, TestData}
import shared.UpdaterHelpers.ListStringUpdater

object DayFifteen extends DayChallenge[Int, Int] with Helpers {
  override def partOne(l: List[String]): Int =
    l.flatMap(_.split(",")).map(hash).sum

  override def partTwo(l: List[String]): Int =
    fillBoxes(instructions(l)).foldLeft(Map[String, Int]()) { case (mapOfLabelsToScore, (boxIndex, lenses)) =>
      val valuesToUpdate = lenses.zipWithIndex.map { case ((label, power), lensIndex) => label -> score(power, lensIndex, boxIndex) }
      valuesToUpdate.foldLeft(mapOfLabelsToScore) { case (m, (label, score)) => m.updated(label, m.getOrElse(label, 0) + score) }
    }.values.sum

  private def score(power: Int, lensIndex: Int, boxIndex: Int) = (boxIndex + 1) * (lensIndex + 1) * power

  private def instructions(l: List[String]) = l.flatMap(_.split(","))

  private def hash(string: String): Int =
    string.foldLeft(0)((currentValue, char) => ((currentValue + char.toInt) * 17) % 256)

  private def fillBoxes(instructions: List[String], boxes: Map[Int, List[(String, Int)]] = Map()): Map[Int, List[(String, Int)]] =
    instructions.foldLeft(boxes) { case (currentBoxes, firstInstruction) =>
        val label = firstInstruction.split("=|-")(0)
        val box = hash(label)
        if (firstInstruction.contains("="))
          val lensNumber = firstInstruction.split("=")(1).toInt
          addLens(label, lensNumber, box, currentBoxes)
        else
          removeLens(label, box, currentBoxes)
    }

  private def removeLens(label: String, box: Int, currentBoxes: Map[Int, List[(String, Int)]]) =
    currentBoxes.get(box).map(l => l -> l.find(_._1 == label).map(l.indexOf)) match
      case Some(lenses, Some(i)) =>
        val (head, tail) = lenses.splitAt(i)
        currentBoxes.updated(box, head ++ tail.tail)
      case _ => currentBoxes

  private def addLens(label: String, lensNumber: Int, box: Int, currentBoxes: Map[Int, List[(String, Int)]]) =
    currentBoxes.get(box).map(l => l -> l.find(_._1 == label).map(l.indexOf)) match
      case Some(lenses, Some(index)) => currentBoxes.updated(box, lenses.updated(index, (label, lensNumber)))
      case Some(lenses, None) => currentBoxes.updated(box, lenses :+ (label, lensNumber))
      case None => currentBoxes.updated(box, List((label, lensNumber)))
}

object DayFifteenData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
  )
  override val expectedPartOne: Option[Int] = Some(1320)
  override val expectedPartTwo: Option[Int] = Some(145)
}