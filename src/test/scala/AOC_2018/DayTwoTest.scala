package AOC_2018

import org.scalatest.{FlatSpec, Matchers}
import shared.FileReader

class DayTwo2018Test extends FlatSpec with Matchers with FileReader {
  "Day Two" should "do part one properly" in {
    val input = List(
      "abcdef",
      "bababc",
      "abbcde",
      "abcccd",
      "aabcdd",
      "abcdee",
      "ababab"
    )
    DayTwo.partOne(input) shouldBe 12
  }

  it should "do part one with real data" in {
    DayTwo.partOne(realInput) shouldBe 406
  }

  it should "do part two properly" in {
    val input = List(
      "abcde",
      "fghij",
      "klmno",
      "pqrst",
      "fguij",
      "axcye",
      "wvxyz"
    )
    DayTwo.partTwo(input) shouldBe "fgij"
  }

  it should "do part two with real data" in {
    DayTwo.partTwo(realInput) shouldBe "tzyvunogzariwkpcbdewmjhxi"
  }

  val realInput = getRealInput(2018, "DayTwoInput")

}
