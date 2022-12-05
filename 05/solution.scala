import scala.io.Source
import scala.collection.mutable.Buffer


type StacksBuffer = Buffer[List[scala.Char]]

case class Rearrangement(quantity: Int, from: Int, to: Int)


object Main {
    def main(args: Array[String]) = {
        val crateMover9000reducer = (buffer: StacksBuffer, rearrangement: Rearrangement) => {
            (1 to rearrangement.quantity) foreach (_ =>
                buffer(rearrangement.to) +:= buffer(rearrangement.from).head
                buffer(rearrangement.from) = buffer(rearrangement.from).tail
            )
            buffer
        }
        val problem1 = operate(crateMover9000reducer)
        println(s"With CrateMover 9000, the message is $problem1")
        val crateMover9001reducer = (buffer: StacksBuffer, rearrangement: Rearrangement) => {
            buffer(rearrangement.to) = buffer(rearrangement.from).take(rearrangement.quantity) ++ buffer(rearrangement.to)
            buffer(rearrangement.from) = buffer(rearrangement.from).drop(rearrangement.quantity)
            buffer
        }
        val problem2 = operate(crateMover9001reducer)
        println(s"With CrateMover 9001, the message is $problem2")
    }
    def operate(reducer: (StacksBuffer, Rearrangement) => StacksBuffer): String = {
        val Array(rawStacks, rawRearrangements) =
        Source
            .fromFile("input")
            .mkString
            .split("\n\n")
        initRearrangements(rawRearrangements)
            .foldLeft(initStacks(rawStacks))(reducer(_, _))
            .flatMap(_.headOption)
            .mkString
    }
    def initStacks(rawStacks: String): StacksBuffer = {
        val lines =
        rawStacks
            .split("\n")
            .reverse

        val stacks = 
        lines(0)
            .trim
            .split("\\s+")
            .map(_ => List.empty[Char])
            .toBuffer

        lines
            .drop(1)
            .foreach(line =>
                (0 to stacks.length - 1)
                .flatMap(pos =>
                    Some(line(4 * pos + 1))
                    .filter(_ != ' ')
                    .map((pos, _))
                )
                .foreach(stacks(_) +:= _)
            )
        stacks
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
