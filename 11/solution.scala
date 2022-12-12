import scala.io.Source


def ints(s: String) = "\\d+".r.findAllIn(s).map(_.toInt)


case class Monkey(operation: BigInt => BigInt, recipient: BigInt => Int, modulo: BigInt)


object Main {
    val operationPattern = "Operation: new = (\\w+) ([+*]) (\\w+)".r
    val (monkeys, initState) = initialize
    val modulo = monkeys.map(_.modulo).reduce(_ * _)
    def main(args: Array[String]) = {
        val problem1 = compute(20, _ / 3)
        println(s"The level of monkey business after 20 rounds is $problem1")
        val problem2 = compute(10_000, _ % modulo)
        println(s"The level of monkey business after 10000 rounds is $problem2")
    }
    def initialize: (Vector[Monkey], Vector[Vector[BigInt]]) = {
        Source
            .fromFile("input")
            .mkString
            .split("\n\n")
            .map(raw =>
                val lines = raw.linesIterator.drop(1).toVector
                val modulo = ints(lines(2)).next
                val operation = (old: BigInt) => {
                    val parseOpd = (operand: String) => if (operand == "old") old else BigInt(operand)
                    lines(1).trim match
                        case operationPattern(left, "+", right) => parseOpd(left) + parseOpd(right)
                        case operationPattern(left, "*", right) => parseOpd(left) * parseOpd(right)
                }
                val recipient = (level: BigInt) => ints(lines(if (level % modulo == 0) 3 else 4)).next
                (Monkey(operation, recipient, modulo), ints(lines(0)).map(BigInt(_)).toVector)
            )
            .toVector
            .unzip
    }
    def compute(rounds: Int, postOperation: BigInt => BigInt) = {
        val activity = monkeys.map(_ => 0).toArray
        val state = initState.toArray

        for {
            round <- 0 until rounds
            turn <- 0 until monkeys.length
            item <- state(turn)
        } {
                val level = postOperation(monkeys(turn).operation(item))
                val recipient = monkeys(turn).recipient(level)
                state(turn) = state(turn).drop(1)
                state(recipient) = state(recipient) :+ level
                activity(turn) = activity(turn) + 1
        }
        activity.sorted.takeRight(2).map(BigInt(_)).reduce(_ * _)
    }
}
