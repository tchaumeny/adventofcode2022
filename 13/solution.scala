import scala.io.Source
import scala.collection.mutable.Buffer


object Main {
    def main(args: Array[String]) = {
        println(s"The sum of those indices is $problem1")
        println(s"The decoder key is $problem2")
    }

    def problem1 = {
        Source
            .fromFile("input")
            .mkString
            .split("\n\n")
            .map(_.split("\n").map(parse) match
                case Array(left, right) => compare(left, right) == 1
            )
            .zipWithIndex
            .filter(_._1)
            .map(_._2 + 1)
            .reduce(_ + _)
    }

    def problem2 = {
        val markers = List(Buffer(Buffer(2)), Buffer(Buffer(6)))
        val packets = (Source
            .fromFile("input")
            .getLines
            .filterNot(_.trim.isEmpty)
            .toList
            .map(parse)
            ++ markers)
            .sortWith((l, r) => compare(l, r) == 1)

        packets.zipWithIndex.filter(markers contains _._1).map(_._2 + 1).reduce(_ * _)
    }

    def parse(line: String): Buffer[Matchable] = {
        var stack = List.empty[Buffer[Matchable]]
        for (token <- """\d+|[,\[\]]""".r.findAllIn(line))
            token match
                case "[" => {
                    val buffer = Buffer.empty[Matchable]
                    stack.headOption.foreach(_ += buffer)
                    stack +:= buffer
                }
                case "]" => if (!stack.tail.isEmpty) stack = stack.tail
                case "," =>
                case digits => stack.head.append(digits.toInt)
        stack.head
    }

    def compare(left: Matchable, right: Matchable): Int = {
        (left, right) match
            case (left: Int, right: Int) => {
                if (left < right) 1
                else if (left > right) -1
                else 0
            }
            case (left: Buffer[Matchable @unchecked], right: Buffer[Matchable @unchecked]) => {
                (left zip right)
                .map((subl, subr) => compare(subl, subr))
                .find(_ != 0)
                .getOrElse(
                    if (left.length < right.length) 1
                    else if (left.length > right.length) -1
                    else 0
                )
            }
            case (left: Buffer[Matchable @unchecked], right: Int) => compare(left, Buffer(right))
            case (left: Int, right: Buffer[Matchable @unchecked]) => compare(Buffer(left), right)
    }
}
