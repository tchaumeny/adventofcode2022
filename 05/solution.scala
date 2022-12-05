import scala.io.Source


class StacksArrangement(stacks: Array[List[String]]):
    def message = stacks.flatMap(_.headOption).mkString
    def rearrange(rearrangement: Rearrangement): StacksArrangement = {
        val buffer = stacks.toBuffer
        (1 to rearrangement.quantity) foreach (_ =>
            buffer(rearrangement.to) +:= buffer(rearrangement.from).head
            buffer(rearrangement.from) = buffer(rearrangement.from).tail
        )
        StacksArrangement(buffer.toArray)
    }
    def rearrange2(rearrangement: Rearrangement): StacksArrangement = {
        val buffer = stacks.toBuffer
        buffer(rearrangement.to) = buffer(rearrangement.from).take(rearrangement.quantity) ++ buffer(rearrangement.to)
        buffer(rearrangement.from) = buffer(rearrangement.from).drop(rearrangement.quantity)
        StacksArrangement(buffer.toArray)
    }
end StacksArrangement


case class Rearrangement(quantity: Int, from: Int, to: Int)


object Main {
    def main(args: Array[String]) = {
        println(s"The message on top of the stacks is $problem1")
        println(s"With CrateMover 9001, the message is $problem2")
    }
    def problem1 = {
        val Array(rawStacks, rawRearrangements) = 
        Source
            .fromFile("input")
            .mkString
            .split("\n\n")
        initRearrangements(rawRearrangements)
            .foldLeft(initStacks(rawStacks))(_ rearrange _)
            .message
    }
    def problem2 = {
        val Array(rawStacks, rawRearrangements) =
        Source
            .fromFile("input")
            .mkString
            .split("\n\n")
        initRearrangements(rawRearrangements)
            .foldLeft(initStacks(rawStacks))(_ rearrange2 _)
            .message
    }
    def initStacks(rawStacks: String): StacksArrangement = {
        val lines =
        rawStacks
            .split("\n")
            .reverse

        val stacks = 
        lines(0)
            .split("\\s+")
            .map(_ => List.empty[String])
            .toBuffer

        val cratePattern = "\\[([A-Z])\\]\\s*".r
        lines
            .drop(1)
            .foreach(_
                .grouped(4)
                .map{
                    case cratePattern(label) => Some(label)
                    case _ => None
                }
                .zipWithIndex
                .foreach((label, pos) =>
                    label
                    .foreach(
                        stacks(pos) +:= _
                    )
                )
            )
        StacksArrangement(stacks.toArray)
    }
    def initRearrangements(rawRearrangements: String): Array[Rearrangement] = {
        val rearrangementPattern = "move (\\d+) from (\\d+) to (\\d+)".r
        rawRearrangements
            .split("\n")
            .map{
                case rearrangementPattern(qtity, from, to) => Rearrangement(qtity.toInt, from.toInt - 1, to.toInt - 1)
            }
    }
}
