import scala.io.Source


case class Coords(x: Int, y: Int) {
    def +(other: Coords) = Coords(x + other.x, y + other.y)
    def -(other: Coords) = Coords(x - other.x, y - other.y)
    def distance(other: Coords) = (x - other.x).abs.max((y - other.y).abs)
}

case class State(head: Coords, tail: Coords) {
    def step(move: Coords): State = {
        val newHead = head + move
        val newTail = {
            val diff = newHead - tail
            if (tail.distance(newHead) <= 1) tail
            else tail + Coords(diff.x.sign, diff.y.sign)
        }
        State(newHead, newTail)
    }
}

object Main {
    val origin = Coords(0, 0)
    def main(args: Array[String]) = {
        println(s"The tail visited $problem1 positions.")
    }
    def problem1 = {
        Source
            .fromFile("input")
            .getLines
            .map(_.split(" "))
            .flatMap{case Array(dir, repeat) => List.fill(repeat.toInt)(
                dir match
                    case "U" => Coords(0, 1)
                    case "D" => Coords(0, -1)
                    case "L" => Coords(-1, 0)
                    case "R" => Coords(1, 0)
            )}
            .scanLeft(State(origin, origin))(_ step _)
            .map(_.tail)
            .distinct
            .length
    }
}
