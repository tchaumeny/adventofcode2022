import scala.io.Source


object Main {
    def main(args: Array[String]): Unit = {
        val calories = Source
                        .fromFile("dataset.txt")
                        .mkString
                        .split("\n\n")
                        .map(_
                            .split("\n")
                            .map(_.toInt)
                            .sum
                        )
                        .sorted
                        .reverse

        println(s"The elf carrying the most calories carries ${calories.head} calories")
        println(s"The 3 elves carrying the most calories carry ${calories.take(3).sum} calories")
    }
}
