package AOC_2017

import shared.DayChallenge

object DaySix extends DayChallenge[Int, Int] {

  override def partOne(l: List[String]): Int =
    redistributeMemoryBlocks(l.head.split("\t").map(_.toInt).toList).size

  def redistributeMemoryBlocks(startingMemoryBlocks: List[Int]): List[List[Int]] = {
    val size = startingMemoryBlocks.size

    //YUCK
    def go(memoryBlocks: List[Int], previous: List[List[Int]]): List[List[Int]] = {
      val indexOfToRedistribute = memoryBlocks.indexOf(memoryBlocks.max)
      val numberGettingFull = if (memoryBlocks.max % size == 0) size else memoryBlocks.max % size
      val maxToRedistribute = (memoryBlocks.max.toDouble / size).ceil.toInt
      val indicesGettingFull =
        (indexOfToRedistribute + 1 until indexOfToRedistribute + 1 + numberGettingFull).toList.map(i => i % size)
      val memoryBlocksWithoutMax = memoryBlocks.updated(indexOfToRedistribute, 0)
      val newMemoryBlocks = (0 until size).toList.map(i =>
        if (indicesGettingFull.contains(i)) memoryBlocksWithoutMax(i) + maxToRedistribute
        else memoryBlocksWithoutMax(i) + maxToRedistribute - 1
      )
      if (previous.contains(newMemoryBlocks)) {
        previous :+ newMemoryBlocks
      } else {
        go(newMemoryBlocks, previous :+ newMemoryBlocks)
      }
    }
    go(startingMemoryBlocks, List.empty)
  }

  override def partTwo(l: List[String]): Int = {
    val result = redistributeMemoryBlocks(l.head.split("\t").map(_.toInt).toList)
    result.size - 1 - result.indexOf(result.last)
  }

  override val expectedPartOne: Option[Int] = Some(5)
  override val expectedPartTwo: Option[Int] = Some(4)
  override val testData: List[String] = List("0\t2\t7\t0")
}
