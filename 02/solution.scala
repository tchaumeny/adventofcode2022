import java.lang.Math.floorMod
import scala.io.Source


object Main {
    val aliases = List("ABC", "XYZ")
    def main(args: Array[String]) = {
        val score1 = computeScore({ case Array(a, b) =>
            (if (floorMod(b - a, 3) == 1) 6 else if (a == b) 3 else 0) + b + 1
        })
        println(s"Score computed with first method: $score1")

        val score2 = computeScore({ case Array(a, b) =>
            if (b == 0) {
                floorMod(a - 1, 3) + 1
            } else if (b == 1) {
                a + 1 + 3
            } else {
                floorMod(a + 1, 3) + 1 + 6
            }
        })
        println(s"Score computed with second method: $score2")
    }
    def computeScore(calculator: Array[Int] => Int): Int = {
        Source
            .fromFile("input")
            .mkString
            .split("\n")
            .map(_
                .split(" ")
                .zipWithIndex
                .map{ case (letter, player) => aliases(player).indexOfSlice(letter)}
            )
            .map(calculator)
            .sum
    }
}
