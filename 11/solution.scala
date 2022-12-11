import scala.io.Source


def ints(s: String) = "\\d+".r.findAllIn(s).map(_.toInt)


case class Monkey(operation: Int => Int, recipient: Int => Int, modulo: Int)


object Main {
    val operationPattern = "Operation: new = (\\w+) ([+*]) (\\w+)".r
    def main(args: Array[String]) = {
        println(s"The level of monkey business after 20 rounds is $problem1")
        println(s"The level of monkey business after 10000 rounds is $problem2")
    }
    def initialize: (Array[Monkey], Array[Vector[Int]]) = {
        Source
            .fromFile("input")
            .mkString
            .split("\n\n")
            .map(raw =>
                val lines = raw.linesIterator.drop(1).toVector
                val modulo = ints(lines(2)).next
                val operation = (old: Int) => {
                    val parseOpd = (operand: String) => if (operand == "old") old else operand.toInt
                    lines(1).trim match
                        case operationPattern(left, "+", right) => parseOpd(left) + parseOpd(right)
                        case operationPattern(left, "*", right) => parseOpd(left) * parseOpd(right)
                }
                val recipient = (level: Int) => ints(lines(if (level % modulo == 0) 3 else 4)).next
                (Monkey(operation, recipient, modulo), ints(lines(0)).toVector)
            )
            .unzip
    }
    def problem1 = {
        val (monkeys, state) = initialize
        val activity = monkeys.map(_ => 0)

        for (round <- 0 until 20) {
            for {
                turn <- 0 until monkeys.length
                item <- state(turn)
            } {
                val level = monkeys(turn).operation(item) / 3
                val recipient = monkeys(turn).recipient(level)
                state(turn) = state(turn).drop(1)
                state(recipient) = state(recipient) :+ level
                activity(turn) = activity(turn) + 1
            }
            //println(s"State after round ${round + 1}:")
            //state.zipWithIndex.foreach((items, idx) =>
            //    println(s"$idx: ${items.mkString(", ")}")
            //)
        }
        activity.sorted.takeRight(2).reduce(_ * _)
    }
    def problem2 = {
        val (monkeys, initState) = initialize
        val activity = monkeys.map(_ => 0)

        val state = initState
            .map(
                _
                .map(item =>
                    monkeys.map(item % _.modulo)
                )
            )

        for (round <- 0 until 10_000) {
            for {
                turn <- 0 until monkeys.length
                item <- state(turn)
            } {
                val levels = (0 until monkeys.length).map(monkey =>
                    (monkeys(turn).operation(item(monkey)) % monkeys(monkey).modulo)
                ).toArray
                val recipient = monkeys(turn).recipient(levels(turn))
                state(turn) = state(turn).drop(1)
                state(recipient) = state(recipient) :+ levels
                activity(turn) = activity(turn) + 1
            }
            //println(s"State after round ${round + 1}:")
            //state.zipWithIndex.foreach((items, idx) =>
            //    println(s"$idx: ${items.map(_.mkString(":")).mkString(", ")}")
            //)
        }
        activity.sorted.map(BigInt(_)).takeRight(2).reduce(_ * _)
    }
}
