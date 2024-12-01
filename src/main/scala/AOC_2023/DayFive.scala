package AOC_2023

import shared.{DayChallenge, Helpers, TestData}

import scala.annotation.tailrec

case class ConversionMap(destination: Long, source: Long, n: Long)

object DayFive extends DayChallenge[Long, Long] with Helpers{
  override def partOne(l: List[String]): Long =
    val (seeds, transformations) = parse(l)
    seeds.map(getLocation(_, transformations)).min

  private def getLocation(seed: Long, stages: List[List[ConversionMap]]) =
    stages.foldLeft(seed)((currentSeed, stage) => applyStage(currentSeed, stage))

  private def applyStage(seed: Long, stage: List[ConversionMap]): Long =
    stage.find(cm => seed >= cm.source && seed < (cm.source + cm.n)) match
      case Some(conversionMap) => seed + (conversionMap.destination - conversionMap.source)
      case None => seed

  private def parse(l: List[String]) =
    val (seedsLines, otherLines) = l.splitAt(2)
    val transformations = splitIntoGroupsOfList(otherLines).map(_.tail.map({transformationGroup =>
        val numbers = extractLongs(transformationGroup)
        ConversionMap(numbers.head, numbers(1), numbers(2))
    }).toList)
    (extractLongs(seedsLines.head), transformations)

  override def partTwo(l: List[String]): Long = {
    val (seeds, transformations) = parse(l)
    val seedRanges = seeds.grouped(2).map(seedRanges => (seedRanges.head, seedRanges(1)))
    seedRanges.map((start, remaining) => {
      findMin(start, remaining - 1, Long.MaxValue, transformations)
    }).min
  }

  @tailrec
  private def findMin(seed: Long, remaining: Long, minSoFar: Long, transformations: List[List[ConversionMap]]): Long =
    if (remaining == 0)
      minSoFar
    else
      val newMinSoFar = List(getLocation(seed, transformations), minSoFar).min
      findMin(seed + 1, remaining - 1, newMinSoFar, transformations)
}

object DayFiveData extends TestData[Long, Long] {
  override val testData: List[String] = List(
    "seeds: 79 14 55 13",
      "",
      "seed-to-soil map:",
      "50 98 2",
      "52 50 48",
      "",
      "soil-to-fertilizer map:",
      "0 15 37",
      "37 52 2",
      "39 0 15",
      "",
      "fertilizer-to-water map:",
      "49 53 8",
      "0 11 42",
      "42 0 7",
      "57 7 4",
      "",
      "water-to-light map:",
      "88 18 7",
      "18 25 70",
      "",
      "light-to-temperature map:",
      "45 77 23",
      "81 45 19",
      "68 64 13",
      "",
      "temperature-to-humidity map:",
      "0 69 1",
      "1 0 69",
      "",
      "humidity-to-location map:",
      "60 56 37",
      "56 93 4"
  )
  override val expectedPartOne: Option[Long] = Some(35L)
  override val expectedPartTwo: Option[Long] = Some(46L)
}