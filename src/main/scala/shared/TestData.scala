package shared

trait TestData[+A, +B] {
  val testData: List[String]
  val testData2: Option[List[String]] = None
  val expectedPartOne: Option[A]
  val expectedPartTwo: Option[B]
}
