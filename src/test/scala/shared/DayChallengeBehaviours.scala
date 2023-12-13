package shared

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

trait DayChallengeBehaviours[A, B] extends Matchers with FileReader with TableDrivenPropertyChecks {
  this: AnyFlatSpec =>
  val day: String
  val file: String
  val dayChallenge: DayChallenge[A, B]
  val year: Int = 2020
  final def partOneWorkingCorrectly(l: List[String], expected: A): Unit = {
    day should "part one should work on test data" in {
      dayChallenge.partOne(l) shouldBe expected
    }
  }

  final def partTwoWorksOnTestData(l: List[String], expected: B): Unit = {
    it should "part two should work on test data" in {
      dayChallenge.partTwo(l) shouldBe expected
    }
  }

  final def partOneWorksOnRealData(expected: A): Unit = {
    it should "part one should work on real data" in {
      dayChallenge.partOne(realData) shouldBe expected
    }
  }


  final def partTwoWorksOnRealData(expected: B): Unit = {
    it should "part two should work on real data" in {
      dayChallenge.partTwo(realData) shouldBe expected
    }
  }

  lazy val realData = getRealInput(year, file)

}

