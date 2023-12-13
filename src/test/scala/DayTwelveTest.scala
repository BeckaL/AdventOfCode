import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import AOC_2023.DayTwelve.countPossibilitiesNew
class DayTwelve2023Test extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {

  "work out number of combinations" should "correctly work out the possible combinations" in {
    val data = Table(
      ("string", "match numbers", "number of possibilities"),
      ("?", List(3), 0),
      ("??", List(1, 1), 0),
      ("?", List(1), 1),
      ("??", List(2), 1),
      ("??", List(1), 2),
      ("???.??", List(3, 1), 2),
      ("?....", List(1), 1),
      ("?.?...", List(2), 0),
      ("#?", List(1), 1),
      (".?", List(1), 1),
      ("#???", List(2), 1),
      ("#?.?", List(2, 1), 1),
      ("###.?", List(2, 1), 0),
      ("??#.", List(3), 1),
      ("?.#.", List(3), 0),
      ("???", List(1, 1), 1),
      ("???.###", List(1,1,3), 1),
      (".??..??...?##.", List(1,1,3), 4),
      ("?#?#?#?#?#?#?#?", List(1,3,1,6), 1),
      ("????.#...#...", List(4,1,1), 1),
      ("????.######..#####.", List(1,6,5), 4),
      ("?###????????", List(3,2,1), 10),
//      ("???#..##.", List(2, 2), 1),
//      ("????????#..##.", List(1, 2, 2, 2), 6),
    )

    data.forEvery { case (string, numbers, expectedNumberOfPossibilities) =>
      countPossibilitiesNew(string, numbers) shouldBe expectedNumberOfPossibilities
    }
  }
}
