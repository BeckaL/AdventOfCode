package AOC_2018

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import shared.FileReader

class DayOne2018Test extends AnyFlatSpec with Matchers with FileReader {
  "Day One" should "do part one properly" in {
    val input = List(1721, 979, 366, 299, 675, 1456).map(_.toString)
    DayOne.partOne(input) shouldBe 3
  }

  it should "do part one with real data" in {
    DayOne.partOne(realInput) shouldBe 406
  }

  it should "do part two properly" in {
    val input = List("+7", "+7", "-2", "-7", "-4")
    DayOne.partTwo(input) shouldBe 14
  }

  it should "do part two with real data" in {
    DayOne.partTwo(realInput) shouldBe 312
  }

  val realInput = getRealInput(2018, "DayOneInput")

}
