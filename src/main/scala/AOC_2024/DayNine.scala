package AOC_2024

import shared.{DayChallenge, TestData, UpdaterHelpers}

import scala.annotation.tailrec

object DayNine extends DayChallenge[Long, Long] {
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
      val (newSoFar, newChunks, newSpaces, newIndexInFile) =
        moveChunk(chunks.last, chunks.dropRight(1), spaces, soFar, indexInFile)
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

  @tailrec
  private def defragMaintainingFiles(wholeMemory: List[Option[Int]], intsToWholeMemoryIndices: Map[Int, Int], remainingIndicesToTry: List[Int]): List[Option[Int]] =
    remainingIndicesToTry match
      case Nil => wholeMemory
      case index :: _ =>
        println(s"now on index $index")
        val newMemory = placeMemoryChunk(intsToWholeMemoryIndices(index), wholeMemory)
        defragMaintainingFiles(newMemory, intsToWholeMemoryIndices, remainingIndicesToTry.tail)

  private def calculateFinal(wholeMemory: List[Option[Int]]) =
    wholeMemory.zipWithIndex.map{ case (maybeChunkInitialPosition, endPosition) =>
      maybeChunkInitialPosition match
        case None => 0
        case Some(chunkInitialPosition) => chunkInitialPosition.toLong * endPosition
    }.sum

  private def placeMemoryChunk(indexToStartAt: Int, wholeMemory: List[Option[Int]]): List[Option[Int]] =
    val memoryChunkInitialPosition = wholeMemory(indexToStartAt).get
    val toMove = wholeMemory.slice(indexToStartAt, wholeMemory.size).takeWhile(maybeInt => maybeInt == Some(memoryChunkInitialPosition))
    val sizeOfToMove = toMove.size
    val maybeIndexToMoveTo = wholeMemory.slice(0, indexToStartAt).sliding(sizeOfToMove).zipWithIndex.find(_._1.forall(_.isEmpty)).map(_._2)
    maybeIndexToMoveTo match
      case None =>
        wholeMemory
      case Some(i) =>
        (0 until sizeOfToMove).foldLeft(wholeMemory){case (mem, offset) =>
        val memoryAfterUpdating = UpdaterHelpers.updateAtIndex(i + offset, Some(memoryChunkInitialPosition), mem)
        UpdaterHelpers.updateAtIndex(indexToStartAt + offset, None, memoryAfterUpdating)
      }

  @tailrec
  private def constructEntireList(chunks: List[MemoryChunk], spaces: List[Int], soFar: List[Option[Int]], m: Map[Int, Int]): (List[Option[Int]], Map[Int, Int]) =
    (chunks, spaces) match {
      case (firstChunk :: _, Nil) =>
        (soFar ++ (0 until firstChunk.size).map(_ => Some(firstChunk.position)), m.updated(firstChunk.position, soFar.size))
      case (firstChunk :: _, firstSpace :: __) =>
        val newSoFar = soFar ++
          (0 until firstChunk.size).map(_ => Some(firstChunk.position)) ++
          (0 until firstSpace).map(_ => None)
        val newM = m.updated(firstChunk.position, soFar.size)
        constructEntireList(chunks.tail, spaces.tail, newSoFar, newM)
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

  override def partTwo(l: List[String]): Long =
    val memoryChunks = parseMemoryChunks(l.head)
    val spaces = parseSpaces(l.head)
    val (wholeStartingMemory, indicesToStartingPoints) = constructEntireList(parseMemoryChunks(l.head), spaces, List(), Map())
    val endMemory = defragMaintainingFiles(wholeStartingMemory, indicesToStartingPoints, (0 to memoryChunks.last.position).reverse.toList)
    calculateFinal(endMemory)
}

object DayNineData extends TestData[Long, Long] {
  override val testData: List[String] = List(
    "2333133121414131402"
  )
  override val expectedPartOne: Option[Long] = Some(1928)
  override val expectedPartTwo: Option[Long] = Some(2858)
}