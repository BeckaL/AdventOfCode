package AOC_2017

import shared.{DayChallenge, Helpers, TestData}

object DayEight extends DayChallenge[Int, Int] with Helpers {
  //TODO: general refactor in here, bit of a mess

  override def partOne(l: List[String]): Int = {
    l.foldRight(Map.empty[String, Int]) { case (stringInstruction, currentMap) =>
      updateStateReturningMapAndNewValue(stringInstruction, currentMap)._1
    }.values.max
  }

  override def partTwo(l: List[String]): Int = {
    l.foldRight((Map.empty[String, Int], Option.empty[Int])) { case (stringInstruction, (currentMap, maybeHighest)) =>
      val (newMap, maybeNewValue) = updateStateReturningMapAndNewValue(stringInstruction, currentMap)
      val newMaybeHighest = (maybeNewValue, maybeHighest) match {
        case (Some(newValue), Some(highest)) => if (newValue < highest) maybeHighest else maybeNewValue
        case (Some(_), None) => maybeNewValue
        case _ => maybeHighest
      }
      (newMap, newMaybeHighest)
    }._2.get
  }


  private def updateStateReturningMapAndNewValue(s: String, map: Map[String, Int]): (Map[String, Int], Option[Int]) = {
    val (modifyString, conditionString) = getTwoFromSplit(s, " if ")
    val modifyInstruction = modifyString.split(" ").toList match {
      case variable :: "inc" :: by :: Nil => Increment(variable, by.toInt)
      case variable :: "dec" :: by :: Nil => Decrease(variable, by.toInt)
      case _ => throw new RuntimeException(s"couldn't split modify instruction ${modifyString}")
    }
    conditionString.split(" ").toList match {
      case variable :: sign :: number :: Nil => interpret(variable, sign, number.toInt, modifyInstruction, map)
      case _ => throw new RuntimeException(s"couldn't split condition instruction ${conditionString}")
    }
  }

  trait ModifyInstruction {
    val variableName: String

    def modify: Int => Int
  }

  case class Increment(override val variableName: String, by: Int) extends ModifyInstruction {
    def modify = v => v + by
  }

  case class Decrease(override val variableName: String, by: Int) extends ModifyInstruction {
    def modify = v => v - by
  }

  private def interpret(variable: String, sign: String, than: Int, modifyInstruction: ModifyInstruction, vals: Map[String, Int]): (Map[String, Int], Option[Int]) = {
    val v = vals.getOrElse(variable, 0)
    if (sign match {
      case ">" => v > than
      case ">=" => v >= than
      case "<" => v < than
      case "<=" => v <= than
      case "==" => v == than
      case "!=" => v != than
    }) {
      val newVal = modifyInstruction.modify(vals.getOrElse(modifyInstruction.variableName, 0))
      (vals.updated(modifyInstruction.variableName, newVal), Some(newVal))
    } else (vals, None)
  }
}

object DayEightData extends TestData[Int, Int] {
  override val expectedPartOne: Option[Int] = Some(1)
  override val testData: List[String] = List(
    "b inc 5 if a > 1",
    "a inc 1 if b < 5",
    "c dec -10 if a >= 1",
    "c inc -20 if c == 10"
  )
  override val expectedPartTwo: Option[Int] = Some(10)
}
