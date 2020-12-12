package AOC_2020

import shared.DayChallenge

object DayTen extends DayChallenge[Int, Long] {
  override val testData: List[String] = List("28",
    "33",
    "18",
    "42",
    "31",
    "14",
    "46",
    "20",
    "48",
    "47",
    "24",
    "23",
    "49",
    "45",
    "19",
    "38",
    "39",
    "11",
    "1",
    "32",
    "25",
    "35",
    "8",
    "17",
    "7",
    "9",
    "4",
    "2",
    "34",
    "10",
    "3")

  override val expectedPartOne: Option[Int] = Some(220)

  override def partOne(l: List[String]): Int = {
    val sorted = l.map(_.toInt).sorted
    val diffs = getDiffs(sorted)

    println(diffs.count(_ == 1))
    println(diffs.count(_ == 3))
    diffs.count(_ == 1) * (diffs.count(_ == 3) + 1)
  }

  //  private def traverse(currentVoltage: Int = 0, currentDiffs: List[Int] = List(), sortedL: List[Int]): Int = {
  //   sortedL.take(0)
  //
  //  }

  private def getDiffs(sortedL: List[Int]): List[Int] =
  sortedL.zipWithIndex.map { case (k, i) => if (i == 0) k - 0 else k - sortedL(i - 1)}

  override val expectedPartTwo: Option[Long] = Some(19208)

  private def getCombinations(l: List[Int], before: Int = 3, after: Int = 3) = {
    val combos = l.toSet.subsets().map(_.toList.sorted)
    combos.count{isValid(_, l, before, after)}
  }

  private def split(l: List[Int]): List[List[Int]] = l.mkString.split("3+").map(_.split("").toList.map(_.toInt)).toList

  private def isValid(subList: List[Int], overallL: List[Int], before: Int = 3, after: Int = 3) = {
    val previousElem = overallL.min - before
    val nextElem = overallL.max + after
    def elemBeforeCondition(k: Int, prevK: Int) = prevK < k && k - prevK < 4
    def elemAfterCondition(k: Int, nextK: Int) = nextK > k && nextK - k < 4
    def firstElemCondition(k: Int) = elemBeforeCondition(k, previousElem)
    def lastElemConidition(k: Int) = elemAfterCondition(k, nextElem)

    if(subList.isEmpty) {
      println(s"is empty")
      println(previousElem)
      println(nextElem)
      elemAfterCondition(previousElem, nextElem)
    } else { subList.zipWithIndex.forall { case (k, i) =>
      val isLastElem = i + 1 == subList.size
      val isFirstElem = i == 0
      (isFirstElem, isLastElem) match {
        case (true, true) =>
          firstElemCondition(k) && lastElemConidition(k)
        case (false, true) =>
          elemBeforeCondition(k, subList(i - 1)) && lastElemConidition(k)
        case (true, false) =>
          elemAfterCondition(k, subList(i + 1)) && firstElemCondition(k)
        case (false, false) =>
          elemAfterCondition(k, subList(i + 1)) && elemBeforeCondition(k, subList(i - 1))
      }
    }
    }
  }

  def getTotRec(joltages: List[Int], cur: Int, mv:Int): Long = {
    if (cur == mv) 1
    else {
      val launchingPoints = (cur+1 to cur+3).filter(joltages contains _)
      launchingPoints.map(getTotRec(joltages, _, mv)).sum
    }
  }

  def getTotalArrangements(joltages: List[Int], cur: Int, mv:Int): Long = {
    val threeGaps = joltages.sorted.sliding(2).filter({case List(a, b) => b-a == 3}).map(_.last).toList :+ (mv+3)
    threeGaps.foldLeft((0, 1:Long))({case ((prev, acc), v) => {
      (v, acc * getTotRec(joltages, prev, v-3))
    }
    })._2
  }

  override def partTwo(l: List[String]): Long = {
    val ints = l.map(_.toInt).sorted
    val diffs = getDiffs(l.map(_.toInt).sorted)
    println(diffs)
    println(split(diffs))
    getTotalArrangements(ints, 0, ints.max)
  }
}
