package AOC_2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import shared.FileReader

class DayTwoTest extends AnyFlatSpec with Matchers with FileReader {
  "Day two" should "work on test data" in {
    DayTwo.partOne(testInput) shouldBe 2
  }

  it should "work on real data" in {
    DayTwo.partOne(realInput) shouldBe 655
  }

  it should "do part two properly" in {
    DayTwo.partTwo(testInput) shouldBe 1
  }

  it should "do part two properly on real data" in {
    DayTwo.partTwo(realInput) shouldBe 673
  }

  val testInput = List("1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc")
  val realInput = getRealInput(2020, "Day2Input")
}
