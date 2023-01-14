import java.lang.Math.floorMod
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
        println(problem2)
    }
    def problem1 = {
        computePassword((position: Coords, direction: Coords) =>
            val nextPos = direction match
                case Coords(0, 1) => grid.keys.filter(_.x == position.x).minBy(_.y)
                case Coords(0, -1) => grid.keys.filter(_.x == position.x).maxBy(_.y)
                case Coords(1, 0) => grid.keys.filter(_.y == position.y).minBy(_.x)
                case Coords(-1, 0) => grid.keys.filter(_.y == position.y).maxBy(_.x)
            (nextPos, direction)
        )
    }
    def problem2 = {
        /*
            Our cube has side 50 and net:
                   
                A B   
                C  
              E D   
              F 
               
        */
        computePassword((position: Coords, direction: Coords) =>
            val L = 50
            val (x, y) = (floorMod(position.x, L), floorMod(position.y, L)) // Position on the square

            position match
                case _ if (L until 2 * L contains position.x) && position.y == 0 =>
                    (Coords(0, 3 * L + x), Coords(1, 0))    // A top -> F left
                case _ if position.x == L && (0 until L contains position.y) =>
                    (Coords(0, 3 * L - 1 - y), Coords(1, 0)) // A left -> E left
                case _ if (2 * L until 3 * L contains position.x) && position.y == 0 =>
                    (Coords(x, 4 * L - 1), Coords(0, -1))  // B top -> F bottom
                case _ if position.x == 3 * L - 1 && (0 until L contains position.y) =>
                    (Coords(2 * L - 1, 3 * L - 1 - y), Coords(-1, 0))  // B right -> D right
                case _ if (2 * L until 3 * L contains position.x) && position.y == L - 1 =>
                    (Coords(2 * L - 1, L + x), Coords(-1, 0)) // B bottom -> C right
                case _ if position.x == L && (L until 2 * L contains position.y) =>
                    (Coords(y, 2 * L), Coords(0, 1))  // C left -> E top
                case _ if position.x == 2 * L - 1 && (L until 2 * L contains position.y) =>
                    (Coords(2 * L + y, L - 1), Coords(0, -1))   // C right -> B bottom
                case _ if position.x == 2 * L - 1 && (2 * L until 3 * L contains position.y) =>
                    (Coords(3 * L - 1, L - 1 - y), Coords(-1, 0)) // D right -> B right
                case _ if (L until 2 * L contains position.x) && position.y == 3 * L - 1 =>
                    (Coords(L - 1, 3 * L + x), Coords(-1, 0)) // D bottom -> F right
                case _ if (0 until L contains position.x) && position.y == 2 * L =>
                    (Coords(L, L + x), Coords(1, 0))    // E top -> C left
                case _ if position.x == 0 && (2 * L until 3 * L contains position.y) =>
                    (Coords(L, L - 1 - y), Coords(1, 0))    // E left -> A left
                case _ if position.x == 0 && (3 * L until 4 * L contains position.y) =>
                    (Coords(L + y, 0), Coords(0, 1))    // F left -> A top
                case _ if (0 until L contains position.x) && position.y == 4 * L - 1 =>
                    (Coords(2 * L + x, 0), Coords(0, 1))    // F bottom -> B top
                case _ if position.x == L - 1 && (3 * L until 4 * L contains position.y) =>
                    (Coords(L + y, 3 * L - 1), Coords(0, -1)) // F right -> D bottom
        )
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
    def computePassword(next: (Coords, Coords) => (Coords, Coords)) = {
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
                    def move(position: Coords, direction: Coords, steps: Int): (Coords, Coords) = {
                        if (steps == 0) (position, direction)
                        else {
                            val (nextPos, nextDir) = if (grid.keySet contains (position + direction)) {
                                (position + direction, direction)
                            } else next(position, direction)
                            if (grid(nextPos) == '#') (position, direction)
                            else move(nextPos, nextDir, steps - 1)
                        }
                    }
                    move(position, direction, i.toInt)
                }
            }
        1000 * (endPosition.y + 1) + 4 * (endPosition.x + 1) + directions.indexOf(endDirection)
    }
}
