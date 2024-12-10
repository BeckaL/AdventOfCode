package AOC_2024

import shared.{DayChallenge, TestData}

import scala.annotation.tailrec

object DayNine extends DayChallenge[Long, Int]{
  case class MemoryChunk(size: Int, position: Int)

  override def partOne(l: List[String]): Long = {
    val firstMemoryChunk :: memoryChunks = parseMemoryChunks(l.head)
    val spaces = parseSpaces(l.head)
    val initialScore = score(List((firstMemoryChunk.size, firstMemoryChunk.position)))
    defrag(memoryChunks, spaces, initialScore, firstMemoryChunk.size)
  }

  private def score(result: Seq[(Int, Int)]): Long =
    result.map{case (initialPosition, positionNow) => initialPosition * positionNow.toLong}.sum

  private def defrag(chunks: List[MemoryChunk], spaces: List[Int], soFar: Long, indexInFile: Int): Long =
    if (chunks.isEmpty) {
      soFar
    } else if (chunks.size == 1) {
      soFar + score((0 until chunks.head.size).map(offset => (chunks.head.position, indexInFile + offset)))
    } else {
      val toMove = chunks.last
      val (newSoFar, newChunks, newSpaces, newIndexInFile) =
        moveChunk(toMove, chunks.dropRight(1), spaces, soFar, indexInFile)
      defrag(newChunks, newSpaces, newSoFar, newIndexInFile)
    }

  @tailrec
  private def moveChunk(chunk: MemoryChunk, others: List[MemoryChunk], spaces: List[Int], soFar: Long, indexInFile: Int): (Long, List[MemoryChunk], List[Int], Int) =
    if (spaces.sum == 0) {
      val addition = score((0 until chunk.size).map(offset => (chunk.position, indexInFile + offset)))
      val newSpaces = spaces
      val newSoFar = soFar + addition
      val newIndexInFile = indexInFile + chunk.size
      (newSoFar, others, newSpaces, newIndexInFile)
    } else if (chunk.size < spaces.head) {
      val newSpaces = (spaces.head - chunk.size) +: spaces.tail
      val additionToSoFar = score((0 until chunk.size).map(offset => (chunk.position, indexInFile + offset)))
      val newSoFar = soFar + additionToSoFar
      val newIndexInFile = indexInFile + chunk.size
      if (others.size == 1) {
        moveChunk(others.head, List.empty, newSpaces, newSoFar, newIndexInFile)
      } else {
        (newSoFar, others, newSpaces, newIndexInFile)
      }
    } else if (chunk.size == spaces.head) {
      val newSpaces = spaces.tail
      val additionToSoFar = score((0 until chunk.size).map(offset => (chunk.position, indexInFile + offset)))
      val newSoFar = soFar + additionToSoFar
      val newIndexInFile = indexInFile + chunk.size
      if (others.size > 0) {
        val scoreOfFirstChunk = score((0 until others.head.size).map(offset => (others.head.position, newIndexInFile + offset)))
        val indexInFileAfterCalculatingFirstChunk = newIndexInFile + others.head.size
        val soFarAfterFirstChunk = newSoFar + scoreOfFirstChunk
        (soFarAfterFirstChunk, others.tail, newSpaces, indexInFileAfterCalculatingFirstChunk)
      } else {
        (newSoFar, others, newSpaces, newIndexInFile)
      }
    } else {
      val newSpaces = spaces.tail
      val newChunk = chunk.copy(size = chunk.size - spaces.head)
      val additionToSoFar = score((0 until spaces.head).map(offset => (chunk.position, indexInFile + offset)))
      val indexInFileAfterMovingFromEnd = indexInFile + spaces.head
      if (others.size == 0) {
        (soFar + additionToSoFar, List(newChunk), List.empty, indexInFileAfterMovingFromEnd)
      } else {
        val scoreOfFirstChunk = score((0 until others.head.size).map(offset => (others.head.position, indexInFileAfterMovingFromEnd + offset)))
        val indexInFileAfterCalculatingFirstChunk = indexInFileAfterMovingFromEnd + others.head.size
        val newSoFar = soFar + additionToSoFar + scoreOfFirstChunk
        moveChunk(newChunk, others.tail, newSpaces, newSoFar, indexInFileAfterCalculatingFirstChunk)
      }
    }

  private def parseSpaces(s: String): List[Int] =
    val onlySpaces = s.zipWithIndex.collect { case (ch, i) if i % 2 != 0 => ch }.mkString
    onlySpaces.map(_.toString.toInt).toList

  private def parseMemoryChunks(s: String): List[MemoryChunk] =
    val onlyMemoryChunks = s.zipWithIndex.collect { case (ch, i) if i % 2 == 0 => ch }.mkString
    onlyMemoryChunks.zipWithIndex.map((char, index) => MemoryChunk(char.toString.toInt, index)).toList


  override def partTwo(l: List[String]): Int = {
    2
  }
}

object DayNineData extends TestData[Long, Int] {
  override val testData: List[String] = List(
    "2333133121414131402"
  )
  override val expectedPartOne: Option[Long] = Some(1928)
  override val expectedPartTwo: Option[Int] = Some(0)
}