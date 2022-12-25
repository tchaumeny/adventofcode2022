import java.lang.Math.floorMod
import scala.io.Source
import scala.math.pow


object Main {
    def main(args: Array[String]) = {
        println(problem1)
    }
    def problem1 = {
        intToSnafu(
            Source
                .fromFile("input")
                .getLines
                .map(snafuToInt)
                .sum
        )
    }
    def snafuToInt(snafu: String): Long = {
        snafu
            .foldLeft(0L){ case (acc, c) =>
                5 * acc + (c match
                    case '-' => -1
                    case '=' => -2
                    case _ => c  - '0'
                )
            }
    }
    def intToSnafu(n: Long, prefix: Boolean = false): String = {
        if (n == 0 && prefix) ""
        else if (n == 0) "0"
        else floorMod(n, 5) match
            case 3 => intToSnafu(n / 5 + 1, true) + "="
            case 4 => intToSnafu(n / 5 + 1, true) + "-"
            case r => intToSnafu(n / 5, true) + r.toString
    }
}
