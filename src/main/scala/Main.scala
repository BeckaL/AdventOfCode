import AOC_2015.Controller2015
import AOC_2017.Controller2017
import AOC_2020._

import scala.io.Source

object AOC {
  //example command: $run [day] [year] [part] where year and part are optional e.g. $run 1 2017 1
  def main(args: Array[String]): Unit = {
    val year = if (args.size > 1) {
      args(1)
    } else {
      "2020"
    }
    val controller = year match {
      case "2020" => Controller2020
      case "2015" => Controller2015
      case "2017" => Controller2017
      case _ => throw new RuntimeException("Didn't understand that year")
    }

    //TODO: Make this relative
    val bufferedSource = Source.fromFile(s"/Users/beckalelew/Projects/AdventOfCode/src/test/resources/$year/Day${args.head}Input.txt")
    val input = bufferedSource.getLines().toList
    bufferedSource.close()
    if (args.size > 2) {
      controller.runPart(args.head, args(2), input)
    } else {
      controller.printAnswers(args.head, input)
    }
  }
}
