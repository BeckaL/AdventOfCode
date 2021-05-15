package shared
trait DayChallenge[+A, +B] {
  def partOne(l: List[String]): A
  def partTwo(l: List[String]): B

  //TODO: remove these from here, should all go in test data
  val testData: List[String] = List()
  val testData2: Option[List[String]] = None
  val expectedPartOne: Option[A] = None
  val expectedPartTwo: Option[B] = None
}

