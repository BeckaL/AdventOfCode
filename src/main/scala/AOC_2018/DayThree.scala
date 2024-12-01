package AOC_2018

import shared.{Coord, DayChallenge}

object DayThree extends DayChallenge[Int, Int] {
  override def partOne(l: List[String]): Int = {
    val rectangles = Rectangles(l.map(Rectangle.from))
    rectangles.coordinates.count(checkIfAppearsAtLeastTwice(_, rectangles.rs, 0))
  }

  private def checkIfAppearsAtLeastTwice(coord: Coord, rectangles: List[Rectangle], known: Int): Boolean =
    rectangles.count(r => r.contains(coord)) >= 2

  override def partTwo(l: List[String]): Int = {
    val idsWithRectangles = l.map(ClaimWithRectangle.from)
    idsWithRectangles.find(hasNoOverlap(_, idsWithRectangles)).get.id
  }

  private def hasNoOverlap(rectangle: ClaimWithRectangle, value: List[ClaimWithRectangle]): Boolean =
    ???


  case class Rectangle(lMargin: Int, tMargin: Int, width: Int, height: Int) {
    val start = Coord(lMargin, tMargin)
    val end = Coord(lMargin + width - 1, tMargin + height - 1)

    def contains(coord: Coord) = (start.x to end.x contains coord.x) && (start.y to end.y contains coord.y)
  }

  case class ClaimWithRectangle(id: Int, rect: Rectangle)

  object ClaimWithRectangle {
    def from(str: String): ClaimWithRectangle = str.split("@").toList match
      case List(claim, rectangle: String) => rectangle.trim.split(":").map(_.trim).toList match
        case List(margins, size: String) =>
          val splitMargins = margins.split(",")
          val splitSize = size.split("x")
          val rectangle = Rectangle(splitMargins(0).toInt, splitMargins(1).toInt, splitSize(0).toInt, splitSize(1).toInt)
          val claimInt: Int = claim.trim.replace("#", "").toInt
          ClaimWithRectangle(claimInt, rectangle)
        case _ => throw new RuntimeException("I didn't understand that")
      case _ => throw new RuntimeException("I didn't understand that")
  }

    object Rectangle {
      def from(str: String): Rectangle = str.split("@")(1).trim.split(":").map(_.trim).toList match {
        case List(margins, size: String) =>
          val splitMargins = margins.split(",")
          val splitSize = size.split("x")
          Rectangle(splitMargins(0).toInt, splitMargins(1).toInt, splitSize(0).toInt, splitSize(1).toInt)
        case _ => throw new RuntimeException("I didn't understand that")
      }
    }

    case class Rectangles(rs: List[Rectangle]) {
      val maxX = rs.maxBy(_.end.x).end.x
      val maxY = rs.maxBy(_.end.y).end.y

      def coordinates = for {
        w <- (0 to maxX).toList
        h <- (0 to maxY).toList
      } yield Coord(w, h)
    }

  }
