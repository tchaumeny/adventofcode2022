import scala.annotation.tailrec
import scala.io.Source


case class Coords(x: Int, y: Int) {
    def rotateRight = Coords(-y, x)
    def rotateLeft = Coords(y, -x)
    def +(other: Coords) = Coords(x + other.x, y + other.y)
}

object Main {
    val directions = List(Coords(1, 0), Coords(0, 1), Coords(-1, 0), Coords(0, -1))
    val (grid, pass) = init
    def main(args: Array[String]) = {
        println(problem1)
    }
    def init: (Map[Coords, Char], String) = {
        val inputs = Source
            .fromFile("input")
            .mkString
            .split("\n\n")

        @tailrec
        def buildMap(lines: Array[String], map: Map[Coords, Char], y: Int): Map[Coords, Char] = {
            if (lines.isEmpty) map
            else buildMap(
                lines.tail,
                map ++ lines.head.zipWithIndex.collect{
                    case (c: ('.' | '#'), x) => Coords(x, y) -> c
                },
                y + 1
            )
        }
        val grid = buildMap(inputs(0).split("\n"), Map.empty, 0)
        (grid, inputs(1))
    }
    def nextCoords(position: Coords, direction: Coords) = {
        if (grid.keySet contains (position + direction)) position + direction
        else {
            direction match
                case Coords(0, 1) => grid.keys.filter(_.x == position.x).minBy(_.y)
                case Coords(0, -1) => grid.keys.filter(_.x == position.x).maxBy(_.y)
                case Coords(1, 0) => grid.keys.filter(_.y == position.y).minBy(_.x)
                case Coords(-1, 0) => grid.keys.filter(_.y == position.y).maxBy(_.x)
        }
    }
    def problem1 = {
        val direction = Coords(1, 0)
        val position = Coords.apply.tupled(
            grid.filter(_._2 == '.')
            .keys
            .map(c => (c.y, c.x))
            .min
            .swap
        )
        val (endPosition, endDirection) = ("""\d+|[RL]""".r.findAllIn(pass))
            .foldLeft((position, direction)){
                case ((position, direction), "L") => (position, direction.rotateLeft)
                case ((position, direction), "R") => (position, direction.rotateRight)
                case ((position, direction), i) => {
                    @tailrec
                    def move(position: Coords, direction: Coords, steps: Int): Coords = {
                        if (steps == 0) position
                        else {
                            val next = nextCoords(position, direction)
                            if (grid(next) == '#') position
                            else move(next, direction, steps - 1)
                        }
                    }
                    (move(position, direction, i.toInt), direction)
                }
            }
        1000 * (endPosition.y + 1) + 4 * (endPosition.x + 1) + directions.indexOf(endDirection)
    }
}
