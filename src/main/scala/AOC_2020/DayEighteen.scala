package AOC_2020

import shared.{DayChallenge, Helpers}

object DayEighteen extends DayChallenge[Long, Long] with Helpers {
  override val expectedPartOne: Option[Long] = Some(26457)
  override val expectedPartTwo: Option[Long] = Some(694173)
  override val testData: List[String] = List(
    "1 + 2 * 3 + 4 * 5 + 6",
    "1 + (2 * 3) + (4 * (5 + 6))",
    "2 * 3 + (4 * 5)",
    "5 + (8 * 3 + 9 + 3 * 4 * 3)",
    "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))",
    "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")

  override def partOne(l: List[String]): Long =
    l.map(evaluateSum(_, evaluatePartsNoPrecedence)).sum

  override def partTwo(l: List[String]): Long =
    l.map(evaluateSum(_, evaluatePartsAdditionPrecedence)).sum

  private def evaluateSum(sumString: String, partEvaluator: List[String] => Long): Long =
    partEvaluator(evaluateParentheses(sumString, partEvaluator).split(" ").toList)

  private def evaluateParentheses(sumString: String, partEvaluator: List[String] => Long): String = {
    if (!sumString.contains("(")) {
      sumString
    } else {
      val (openB, closeB) = getLastOpenBracketWithClose(sumString)
      val replacementValue = partEvaluator(sumString.slice(openB + 1, closeB).split(" ").toList)
      val newString = replaceStringFrom(openB, closeB, sumString, replacementValue)
      if (sumString.count(_ == ')') == 1) newString else evaluateParentheses(newString, partEvaluator)
    }
  }


  private def evaluatePluses(sumParts: List[String]): List[String] =
    if (!sumParts.contains("+")) {
      sumParts
    } else {
      val firstPlus = sumParts.indexOf("+")
      val replacement = evaluateOperation(sumParts(firstPlus - 1), sumParts(firstPlus + 1), "+")
      val newParts = replaceList(sumParts, replacement, firstPlus)
      if (sumParts.count(_ == '+') == 1) newParts else evaluatePluses(newParts)
    }

  private def getLastOpenBracketWithClose(sumString: String): (Int, Int) = {
    val lastOpenBracket = sumString.lastIndexOf('(')
    (lastOpenBracket, indicesOf(sumString, ')').find(closeB => closeB > lastOpenBracket).get)
  }

  private def replaceList(l: List[String], replacement: Long, indexOfOperator: Int): List[String] =
    l.take(indexOfOperator - 1) ++ (replacement.toString +: l.takeRight(l.size - (indexOfOperator + 2)))

  private def replaceStringFrom(start: Int, end: Int, string: String, replacementValue: Long) =
    string.take(start) + replacementValue.toString + string.takeRight(string.size - (end + 1))

  private def evaluatePartsNoPrecedence(parts: List[String]): Long = parts match {
    case n :: Nil => n.toLong
    case _ =>
      val newNumber = evaluateOperation(parts(0), parts(2), parts(1))
      parts.size match {
        case 3 => newNumber
        case _ => evaluatePartsNoPrecedence(newNumber.toString +: parts.takeRight(parts.size - 3))
      }
  }

  private def evaluateOperation(n1: String, n2: String, operator: String): Long =
    operator match {
      case "+" => n1.toLong + n2.toLong
      case "*" => n1.toLong * n2.toLong
      case _ => throw new RuntimeException(s"operator $operator wasn't an addition or mutiplication string")
    }

  private def evaluatePartsAdditionPrecedence(parts: List[String]): Long =
    parts match {
      case elem :: Nil => elem.toLong
      case _ => evaluatePartsNoPrecedence(evaluatePluses(parts))
    }



}
