package AOC_2020

import shared.{DayChallenge, Helpers}

import scala.util.{Success, Try}

object DayFour extends DayChallenge[Int, Int] with Helpers {
  override def partOne(l: List[String]): Int =
    splitIntoGroupsOfString(l).count(isValidBasedOnNumberOfFields)

  override def partTwo(l: List[String]): Int =
    splitIntoGroupsOfString(l).count(p => isValidBasedOnNumberOfFields(p) && getPassportSegments(p).forall(isValidField))

  private def isValidBasedOnNumberOfFields(passport: String): Boolean =
    getPassportSegments(passport).map {
      _.split(":").head
    }.intersect(List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")).size == 7

  private def getPassportSegments(passport: String): List[String] = passport.trim.split(" ").map(_.trim).toList


  def isValidField(field: String): Boolean = {
    def isIntegerInRange(str: String, r: Range) = Try(r contains str.toInt) == Success(true)

    val validYearRanges = Map("byr" -> (1920 to 2002), "iyr" -> (2010 to 2020), "eyr" -> (2020 to 2030))
    val validEyeColours = List("amb", "blu", "brn", "gry", "grn", "hzl", "oth")

    field.split(":").toList match {
      case List(id, y: String) if validYearRanges.keySet contains id => isIntegerInRange(y, validYearRanges(id))
      case List("hgt", lenString: String) =>
        ((lenString contains "cm") && isIntegerInRange(lenString.takeWhile(_ != 'c').mkString, Range(150, 193))) ||
          ((lenString contains "in") && isIntegerInRange(lenString.takeWhile(_ != 'i'), Range(59, 76)))
      case List("pid", id: String) => id.length == 9 && Try(id.toInt).isSuccess
      case List("ecl", colour) => validEyeColours contains colour
      case List("hcl", colour) => colour.head == '#' && "[0-9a-f]{6}".r.matches(colour.tail)
      case List("cid", _) => true
      case _ => false
    }
  }


}
