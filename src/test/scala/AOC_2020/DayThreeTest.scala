package AOC_2020

import org.scalatest.{FlatSpec, Matchers}
import shared.FileReader

class DayThreeTest extends FlatSpec with Matchers with FileReader {
  "Day three" should "calculate part one correctly" in {
    val answer = 7
    DayThree.partOne(testInput) shouldBe answer
  }

  it should "calculate part one correctly with real test input" in {
    val answer = 148
    DayThree.partOne(realInput) shouldBe answer
  }

  it should "calculate part two correctly" in {
    val answer = 336
    DayThree.partTwo(testInput) shouldBe answer
  }


  it should "calculate part two correctly with real test input" in {
    val answer = 727923200
    DayThree.partTwo(realInput) shouldBe answer
  }

  val realInput = getRealInput(2020, "Day3Input")

  val testInput = List[String](
    "..##.......",
  "#...#...#..",
  ".#....#..#.",
  "..#.#...#.#",
  ".#...##..#.",
  "..#.##.....",
  ".#.#.#....#",
  ".#........#",
  "#.##...#...",
  "#...##....#",
  ".#..#...#.#"
  )
}
