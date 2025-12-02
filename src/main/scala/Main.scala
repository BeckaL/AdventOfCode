import AOC_2015.Controller2015
import AOC_2017.Controller2017
import AOC_2020.*
import AOC_2021.Controller2021
import AOC_2022.Controller2022
import AOC_2023.Controller2023
import AOC_2024.Controller2024
import AOC_2025.Controller2025

import scala.io.Source

object AOC {
  //example command: $run [day] [year] [part] where year and part are optional e.g. $run 1 2017 1
  def main(args: Array[String]): Unit = {
    val year = if (args.size > 1) {
      args(1)
    } else {
      "2025"
    }
    val controller = year match {
      case "2020" => Controller2020
      case "2015" => Controller2015
      case "2017" => Controller2017
      case "2021" => Controller2021
      case "2022" => Controller2022
      case "2023" => Controller2023
      case "2024" => Controller2024
      case "2025" => Controller2025
      case y => throw new RuntimeException(s"Didn't understand that year ${y}")
    }

    val bufferedSource = Source.fromFile(s"./src/test/resources/$year/Day${args.head}Input.txt")
    val input = bufferedSource.getLines().toList
    bufferedSource.close()
    if (args.size > 2) {
      controller.runPart(args.head, args(2), input)
    } else {
      controller.printAnswers(args.head, input)
    }
  }
}
