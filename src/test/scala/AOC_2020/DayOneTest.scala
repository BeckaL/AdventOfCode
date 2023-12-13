package AOC_2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import shared.FileReader

class DayOneTest extends AnyFlatSpec with Matchers with FileReader {

  "day one" should "give the correct answer" in {
    val input = List(1721, 979, 366, 299, 675, 1456).map(_.toString)
    DayOne.partOne(input) shouldBe 514579
  }

  it should "give the correct answer to part two" in {
    val input = List(1721, 979, 366, 299, 675, 1456).map(_.toString)
    DayOne.partTwo(input) shouldBe 241861950
  }

  it should "do the full one for part one" in {
    DayOne.partOne(fullInput) shouldBe 224436
  }

  it should "do the full one for part two" in {
    DayOne.partTwo(fullInput) shouldBe 303394260
  }

  val fullInput = getRealInput(2020, "Day1Input")

}
