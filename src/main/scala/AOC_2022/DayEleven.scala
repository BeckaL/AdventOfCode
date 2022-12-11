package AOC_2022

import shared.{DayChallenge, TestData, Helpers}

object DayEleven extends DayChallenge[BigInt, Long] with Helpers {
  override def partOne(l: List[String]): BigInt =
    productOfBusiestMonkeys(monkeys(l), 20, _ / 3)

  override def partTwo(l: List[String]): Long = {
    val ms = monkeys(l)
    val lcd = ms.map(_._2.divBy).product
    productOfBusiestMonkeys(ms, 10000, _ % lcd)
  }

  private def productOfBusiestMonkeys(monkeys: Map[Int, Monkey], iterations: Int, worryReducer: Long => Long): Long =
    (0 until iterations)
      .foldLeft(monkeys) { case (ms, _) => round(ms, worryReducer) }
      .toList
      .sortBy { case (_, m) => m.inspected }.takeRight(2)
      .map(_._2.inspected.toLong)
      .product

  private def round(initalMs: Map[Int, Monkey], reducer: Long => Long): Map[Int, Monkey] =
    initalMs.keys.toList.sorted.foldLeft(initalMs) { case (ms, i) => turn(ms(i), ms, reducer)}

  private def turn(monkey: Monkey, monkeys: Map[Int, Monkey], reducer: Long => Long): Map[Int, Monkey] =
    monkey.items.foldLeft(monkeys) { case (ms, item) =>
      val newWorryLevel = reducer(monkey.op(item))
      val throwTo = if (newWorryLevel % monkey.divBy == 0) ms(monkey.ifTrue) else ms(monkey.ifFalse)
      ms.updated(throwTo.i, throwTo.copy(items = throwTo.items :+ newWorryLevel))
    }.updated(monkey.i, monkey.copy(items = List(), inspected = monkey.inspected + monkey.items.size))

  private def monkeys(l: List[String]): Map[Int, Monkey] =
    splitIntoGroupsOfList(l).map { monkeyLines =>
      val ints = monkeyLines.map(line => extractInts(line))
      val firstIFrom: Int => Int = lineNo =>  ints(lineNo).head
      val monkeyIndex = firstIFrom(0)
      val items = ints(1).map(i => i.toLong)
      val operation = monkeyLines(2).trim().split(" ").takeRight(2).toList match {
        case "*" :: "old" :: Nil => x: Long => x * x
        case "*" :: i :: Nil     => x: Long => x * i.toLong
        case "+" :: "old" :: Nil => x: Long => x + x
        case "+" :: i :: Nil     => x: Long => x + i.toLong
      }
      monkeyIndex -> Monkey(monkeyIndex, items, operation, firstIFrom(3), firstIFrom(4), firstIFrom(5), 0)
    }.toMap

  case class Monkey(i: Int, items: List[Long], op: Long => Long, divBy: Int, ifTrue: Int, ifFalse: Int, inspected: Int)
}

object DayElevenData extends TestData[BigInt, BigInt] {
  override val testData: List[String] = List(
    "Monkey 0:",
    "  Starting items: 79, 98",
    "  Operation: new = old * 19",
    "  Test: divisible by 23",
    "    If true: throw to monkey 2",
    "    If false: throw to monkey 3",
    "",
    "Monkey 1:",
    "  Starting items: 54, 65, 75, 74",
    "  Operation: new = old + 6",
    "  Test: divisible by 19",
    "    If true: throw to monkey 2",
    "    If false: throw to monkey 0",
    "",
    "Monkey 2:",
    "  Starting items: 79, 60, 97",
    "  Operation: new = old * old",
    "  Test: divisible by 13",
    "    If true: throw to monkey 1",
    "    If false: throw to monkey 3",
    "",
    "Monkey 3:",
    "  Starting items: 74",
    "  Operation: new = old + 3",
    "  Test: divisible by 17",
    "    If true: throw to monkey 0",
    "    If false: throw to monkey 1",
  )
  override val expectedPartOne: Option[BigInt] = Some(BigInt(10605))
  override val expectedPartTwo: Option[BigInt] = Some(2713310158L)
}