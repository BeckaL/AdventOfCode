package AOC_2024

import shared.{DayChallenge, TestData}

import scala.annotation.tailrec

object DayNine extends DayChallenge[Long, Int]{
  case class MemoryChunk(size: Int, position: Int)

  override def partOne(l: List[String]): Long = {
    val memoryChunks = parseMemoryChunks(l.head)
    val spaces = parseSpaces(l.head)
    defrag(memoryChunks.tail, spaces, 0, memoryChunks.head.size)
  }

  private def score(position: Int, startPosition: Int, length: Int) =
    (0 until length).map(offset => position * (startPosition + offset).toLong).sum

  private def defrag(chunks: List[MemoryChunk], spaces: List[Int], soFar: Long, indexInFile: Int): Long =
    if (chunks.isEmpty) {
      soFar
    } else if (chunks.size == 1) {
      soFar + score(chunks.head.position, indexInFile, chunks.head.size)
    } else {
      val toMove = chunks.last
      val (newSoFar, newChunks, newSpaces, newIndexInFile) =
        moveChunk(toMove, chunks.dropRight(1), spaces, soFar, indexInFile)
      defrag(newChunks, newSpaces, newSoFar, newIndexInFile)
    }

  @tailrec
  private def moveChunk(chunk: MemoryChunk, others: List[MemoryChunk], spaces: List[Int], soFar: Long, indexInFile: Int): (Long, List[MemoryChunk], List[Int], Int) =
    if (spaces.sum == 0) {
      val addition = score(chunk.position, indexInFile, chunk.size)
      val newIndexInFile = indexInFile + chunk.size
      (soFar + addition, others, spaces, newIndexInFile)
    } else if (chunk.size < spaces.head) {
      val newSpaces = (spaces.head - chunk.size) +: spaces.tail
      val (updatedScore, updatedPosition) = updateScoreAndPositionWithChunk(chunk, indexInFile, soFar)
      if (others.size == 1) {
        moveChunk(others.head, List.empty, newSpaces, updatedScore, updatedPosition)
      } else {
        (updatedScore, others, newSpaces, updatedPosition)
      }
    } else if (chunk.size == spaces.head) {
      val newSpaces = spaces.tail
      val (updatedScoreAfterMovingChunk, newIndex) = updateScoreAndPositionWithChunk(chunk, indexInFile, soFar)
      if (others.nonEmpty) {
        val (updatedScore, updatedPosition) = updateScoreAndPositionWithChunk(others.head, newIndex, updatedScoreAfterMovingChunk)
        (updatedScore, others.tail, newSpaces, updatedPosition)
      } else {
        (updatedScoreAfterMovingChunk, others, newSpaces, newIndex)
      }
    } else {
      val newChunk = chunk.copy(size = chunk.size - spaces.head)
      val chunkToMove = chunk.copy(size = spaces.head)
      val (updatedScoreAfterMovingChunk, indexInFileAfterMovingFromEnd) = updateScoreAndPositionWithChunk(chunkToMove, indexInFile, soFar)
      if (others.isEmpty) {
        (updatedScoreAfterMovingChunk, List(newChunk), List.empty, indexInFileAfterMovingFromEnd)
      } else {
        val (updatedScore, updatedPosition) = updateScoreAndPositionWithChunk(others.head, indexInFileAfterMovingFromEnd, updatedScoreAfterMovingChunk)
        moveChunk(newChunk, others.tail, spaces.tail, updatedScore, updatedPosition)
      }
    }

  private def updateScoreAndPositionWithChunk(c: MemoryChunk, indexInFile: Int, soFar: Long): (Long, Int) =
    val scoreOfFirstChunk = score(c.position, indexInFile, c.size)
    (soFar + scoreOfFirstChunk, indexInFile + c.size)


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