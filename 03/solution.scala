import scala.io.Source


object Main {
    val letters = ('a' to 'z') ++ ('A' to 'Z')
    def main(args: Array[String]) = {
        println(s"The sum of priorities of wrong items is $problem1")
        println(s"The sum of priorities of the badges items is $problem2")
    }
    def problem1 = {
        Source
            .fromFile("input")
            .mkString
            .split("\n")
            .map(content => 
                content
                .grouped(content.length / 2)
                .map(_.toSet)
                .reduce(_ & _)
                .head
            )
            .map(letters.indexOf(_) + 1)
            .sum
    }
    def problem2 = {
        Source
            .fromFile("input")
            .mkString
            .split("\n")
            .grouped(3)
            .map(group =>
                group
                .map(_.toSet)
                .reduce(_ & _)
                .head
            )
            .map(letters.indexOf(_) + 1)
            .sum
    }
}