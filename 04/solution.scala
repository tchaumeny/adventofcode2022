import scala.io.Source


object Main {
    def main(args: Array[String]) = {
        val problem1 = countPairsIf((a, b, c, d) => a <= c && b >= d || c <= a && d >= b)
        println(s"The number of pairs where one fully contains the other is $problem1")

        val problem2 = countPairsIf((a, b, c, d) => b >= c && a <= d)
        println(s"The number of pairs with overlap is $problem2")
    }
    def countPairsIf(criteria: (Int, Int, Int, Int) => Boolean) = {
        Source
            .fromFile("input")
            .mkString
            .split("\n")
            .map(_
                .split(",")
                .map(_
                    .split("-")
                    .map(_
                        .toInt
                    )
                )
            ).filter{
                case Array(Array(a, b), Array(c, d)) => criteria(a, b, c, d)
            }
            .length
    }
}
