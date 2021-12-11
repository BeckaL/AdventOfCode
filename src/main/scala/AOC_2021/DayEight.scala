package AOC_2021

import shared.{DayChallenge, TestData}

object DayEight extends DayChallenge[Int, Int] {
  override def partOne(l: List[String]): Int = {
    val noOfSegmentsToDigits = Map(
      6 -> List(0, 6, 9),
      2 -> List(1),
      5 -> List(2, 5, 3),
      4 -> List(4),
      3 -> List(7),
      7 -> List(8)
    )
    val uniqueLengths = noOfSegmentsToDigits.toList.collect { case (k, v) if v.size == 1 => k }
    outputNumbers(l).map(_.count(segments => uniqueLengths.contains(segments.length))).sum
  }

  private def outputNumbers(l: List[String]) =
    l.map(_.split(" \\| ")(1).split(" ").toList)

  private def allNumbers(l: List[String]) =
    l.map { line => line.replace(" | ", " ").split(" ").toList }

  override def partTwo(l: List[String]): Int =
    allNumbers(l).zip(outputNumbers(l)).map { case (rowNumbers, output) => solve(rowNumbers, output) }.sum

  def solve(allNumbers: List[String], outputNumbers: List[String]): Int = {
    def findUnsafeWhereSizeIs(ns: List[String], i: Int) = ns.find(_.size == i).get.sorted

    def findUnsafeWhereSharesNCharsWith(ns: Iterable[String], comparison: String, noInCommon: Int) =
      ns.find(str => comparison.count(str.contains(_)) == noInCommon).get.sorted

    val one               = findUnsafeWhereSizeIs(allNumbers, 2)
    val seven             = findUnsafeWhereSizeIs(allNumbers, 3)
    val eight             = findUnsafeWhereSizeIs(allNumbers, 7)
    val four              = findUnsafeWhereSizeIs(allNumbers, 4)
    val threeFiveTwo      = allNumbers.filter(_.length == 5).map(_.sorted).toSet
    val three             = findUnsafeWhereSharesNCharsWith(ns = threeFiveTwo, comparison = one, noInCommon = 2)
    val sixesNinesZeros   = allNumbers.filter(str => str.size == 6).map(_.sorted).toSet
    val six               = findUnsafeWhereSharesNCharsWith(ns = sixesNinesZeros, comparison = one, noInCommon = 1)
    val five              = findUnsafeWhereSharesNCharsWith(threeFiveTwo, six, 5)
    val possiblesForBAndD = four.filterNot(one.toList.contains)
    val zero              = findUnsafeWhereSharesNCharsWith(sixesNinesZeros, possiblesForBAndD, 1)
    val nine              = sixesNinesZeros.find(str => str != zero && str != six).get.sorted
    val two               = threeFiveTwo.find(s => s != five && s != three).get.sorted

    val m = Map(zero -> 0, one -> 1, two -> 2, three -> 3, four -> 4, five -> 5, six -> 6, seven -> 7, eight -> 8, nine -> 9)

    outputNumbers.map(str => m(str.sorted)).mkString("").toInt
  }
}

object DayEightData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
    "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
    "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
    "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
    "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
    "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
    "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
    "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
    "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
    "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
  )
  override val expectedPartOne: Option[Int] = Some(26)
  override val expectedPartTwo: Option[Int] = Some(61229)
}
