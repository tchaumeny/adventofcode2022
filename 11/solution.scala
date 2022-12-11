import scala.io.Source


def ints(s: String) = "\\d+".r.findAllIn(s).map(_.toInt)


case class Monkey(operation: Int => Int, recipient: Int => Int)


object Main {
    val operationPattern = "Operation: new = (\\w+) ([+*]) (\\w+)".r
    def main(args: Array[String]) = {
        println(s"The level of monkey business after 20 rounds is $problem1")
    }
    def initialize: (Array[Monkey], Array[Vector[Int]]) = {
        Source
            .fromFile("input")
            .mkString
            .split("\n\n")
            .map(raw =>
                val lines = raw.linesIterator.drop(1).toVector
                val operation = (old: Int) => {
                    val parseOpd = (operand: String) => if (operand == "old") old else operand.toInt
                    lines(1).trim match
                        case operationPattern(left, "+", right) => parseOpd(left) + parseOpd(right)
                        case operationPattern(left, "*", right) => parseOpd(left) * parseOpd(right)
                }
                val recipient = (old: Int) => {
                    val divisor = ints(lines(2)).next
                    ints(lines(if (old % divisor == 0) 3 else 4)).next
                }
                (Monkey(operation, recipient), ints(lines(0)).toVector)
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
}
