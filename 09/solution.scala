import scala.io.Source


case class Coords(x: Int, y: Int) {
    def +(other: Coords) = Coords(x + other.x, y + other.y)
    def -(other: Coords) = Coords(x - other.x, y - other.y)
    def distance(other: Coords) = (x - other.x).abs.max((y - other.y).abs)
}


def step(state: List[Coords], move: Coords): List[Coords] = {
    val newHead = state.head + move
    if (state.tail.isEmpty) newHead::Nil
    else newHead::step(
        state.tail,
        {
            val diff = newHead - state.tail.head
            if (state.tail.head.distance(newHead) <= 1) Coords(0, 0)
            else Coords(diff.x.sign, diff.y.sign)
        }
    )
}


object Main {
    def main(args: Array[String]) = {
        println(s"In part 1, the tail visited ${countPositions(2)} positions.")
        println(s"In part 2, the tail visited ${countPositions(10)} positions.")
    }
    def countPositions(knots: Int) = {
        Source
            .fromFile("input")
            .getLines
            .map(_.split(" "))
            .flatMap{case Array(dir, repeat) => List.fill(repeat.toInt)(dir)}
            .map{
                case "U" => Coords(0, 1)
                case "D" => Coords(0, -1)
                case "L" => Coords(-1, 0)
                case "R" => Coords(1, 0)
            }
            .scanLeft(List.fill(knots)(Coords(0, 0)))(step(_, _))
            .map(_.last)
            .distinct
            .length
    }
}
