package AOC_2018

import shared.{DayChallenge, Helpers}

object DayFour extends DayChallenge[Int, Int] with Helpers {
  override def partOne(l: List[String]): Int = {
    println(splitIntoShifts(getTimestampsAndDescriptions(l)).mkString("\n"))
//    println(l.map{ _.replaceAll("\\[", "").replaceAll("\\]","").splitAt(16)})
    0
  }

  private def getTimesAsleep(shift: List[(String,String)], asleep: List[Range] = List()): List[Range] = {
    val asleepStart = shift.find(entry => entry._2 contains "falls asleep")
    val asleepEnd = shift.find(_._2 contains "wakes up")
    shift.indexOf(asleepEnd)
    asleep
  }

  private def splitIntoShifts(timestampsAndDescriptions: List[(String, String)], shifts: List[(Int, List[(String, String)])] = List()): List[(Int, List[(String, String)])] = {
    if (timestampsAndDescriptions.isEmpty) {
      shifts
    } else {
      val shift = timestampsAndDescriptions.takeWhile { case (_, description) =>
        !description.contains("begins shift") || description == timestampsAndDescriptions(0)._2
      }
//      println(s"shift is $shift")
      val id = shift(0)._2.filter(_.isDigit).mkString.toInt
//      println(s"id is $id")
      val remainingTimestampsAndDescriptions = timestampsAndDescriptions.takeRight(timestampsAndDescriptions.size - shift.size)
//      println(s"remainingTimestampsAndDescriptions are $remainingTimestampsAndDescriptions")
      splitIntoShifts(remainingTimestampsAndDescriptions, shifts :+ (id, shift))
    }
  }

  private def getTimestampsAndDescriptions(l: List[String]): List[(String, String)] = {
    l.map{ _.replaceAll("\\[", "").replaceAll("\\]","").splitAt(16)}
  }

  override val testData: List[String] = List(
    "[1518-11-01 00:00] Guard #10 begins shift",
                                          "[1518-11-01 00:05] falls asleep",
                                          "[1518-11-01 00:25] wakes up",
                                          "[1518-11-01 00:30] falls asleep",
                                          "[1518-11-01 00:55] wakes up",
                                          "[1518-11-01 23:58] Guard #99 begins shift",
                                          "[1518-11-02 00:40] falls asleep",
                                          "[1518-11-02 00:50] wakes up",
                                          "[1518-11-03 00:05] Guard #10 begins shift",
                                          "[1518-11-03 00:24] falls asleep",
                                          "[1518-11-03 00:29] wakes up",
                                          "[1518-11-04 00:02] Guard #99 begins shift",
                                          "[1518-11-04 00:36] falls asleep",
                                          "[1518-11-04 00:46] wakes up",
                                          "[1518-11-05 00:03] Guard #99 begins shift",
                                          "[1518-11-05 00:45] falls asleep",
                                          "[1518-11-05 00:55] wakes up")

  override def partTwo(l: List[String]): Int = ???
}
