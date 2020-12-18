import AOC_2015.Controller2015
import AOC_2020._

import scala.io.Source

object AOC {
  def main(args: Array[String]): Unit = {
    val year = if (args.size > 2) {
      args(2)
    } else {
      "2020"
    }
    val controller = year match {
      case "2020" => Controller2020
      case "2015" => Controller2015
      case _ => throw new RuntimeException("Didn't understand that year")
    }
    val bufferedSource = Source.fromFile(s"/Users/rlq3651/Projects/AOC/src/test/resources/$year/Day${args.head}Input.txt")
    val input = bufferedSource.getLines().toList
    bufferedSource.close()
    if (args.size > 1) {
      controller.runPart(args.head, args(1), input)
    } else {
      controller.printAnswers(args.head, input)
    }
  }
}
