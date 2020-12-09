package shared

import scala.io.Source

trait FileReader {
  def getRealInput(year: Int, fileName: String): List[String] = {
    val bufferedSource = Source.fromFile(s"/Users/rlq3651/Projects/AOC/src/test/resources/$year/$fileName.txt")
    val input = bufferedSource.getLines().toList
    bufferedSource.close()
    input
  }
}
