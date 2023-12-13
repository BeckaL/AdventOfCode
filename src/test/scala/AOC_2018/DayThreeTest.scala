package AOC_2018

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import shared.FileReader

class DayThree2018Test extends AnyFlatSpec with Matchers with FileReader {
  "Day three" should "work on test data properly" in {
    DayThree.partOne(testInput) shouldBe 4
  }

  it should "word on real data" in {
    DayThree.partOne(realInput) shouldBe 118223
  }

  it should "do part two" in {
    DayThree.partTwo(testInput) shouldBe 3
  }

  val testInput = List(
    "#1 @ 1,3: 4x4",
    "#2 @ 3,1: 4x4",
    "#3 @ 5,5: 2x2"
  )

  val realInput = getRealInput(2018, "DayThreeInput")
}
