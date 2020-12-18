package shared

trait YearController {
  def runPart(day: String, part: String, input: List[String]): Unit
  def printAnswers(day: String, input: List[String]): Unit
}
