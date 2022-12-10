import scala.io.Source


object Main {
    def main(args: Array[String]) = {
        println(s"The sum of the signal strengths is $problem1")
        println(s"The message is:\n$problem2")
    }
    def sequenceX = {
        Source
            .fromFile("input")
            .getLines()
            .flatMap(_.split(" ") match
                case Array("noop") => List(0)
                case Array("addx", value) => List(0, value.toInt)
            )
            .scanLeft(1)(_ + _)
            .zipWithIndex
    }
    def problem1 = {
        sequenceX
            .filter((_, cycle) => (20 to 220 by 40) contains (cycle + 1))
            .map((acc, cycle) => acc * (cycle + 1))
            .reduce(_ + _)
    }
    def problem2 = {
        sequenceX
            .map((spritePos, cycle) =>
                if ((cycle % 40 - spritePos).abs <= 1) '#'
                else '.'
            )
            .grouped(40)
            .map(_.mkString)
            .mkString("\n")
    }
}
