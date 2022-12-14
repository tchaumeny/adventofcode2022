import scala.annotation.tailrec
import scala.io.Source


case class Coords(x: Int, y: Int) {
    def +(other: Coords) = Coords(x + other.x, y + other.y)
    def *(scalar: Int) = Coords(scalar * x, scalar * y)
    def distance(other: Coords) = (x - other.x).abs.max((y - other.y).abs)
}

object Main {
    val source = Coords(500, 0)
    val initGrid = initialize
    val maxY = initGrid.keys.map(_.y).max
    def main(args: Array[String]) = {
        println(s"$problem1 units of sand come to rest before it flows to the abyss")
        println(s"$problem2 units of sand come to rest before until the source becomes blocked")
    }
    def problem1 = {
        LazyList
            .from(0)
            .scanLeft((initGrid, false)){ case ((grid, abyss), _) =>
                fallWithAbyss(grid, source, maxY) match
                    case Some(position) => (grid + (position -> 'o'), false)
                    case None => (grid, true)
            }
            .takeWhile(!_._2)
            .length - 1
    }
    def problem2 = {
        LazyList
            .from(0)
            .scanLeft((initGrid, false)){ case ((grid, blocked), _) =>
                val position = fallWithFloor(grid, source, maxY + 2)
                (grid + (position -> 'o'), position == source)
            }
            .takeWhile(!_._2)
            .length
    }
    @tailrec
    def fallWithAbyss(grid: Map[Coords, Char], pos: Coords, abyssY: Int): Option[Coords] = {
        List(Coords(0, 1), Coords(-1, 1), Coords(1, 1))
            .map(pos + _)
            .find(grid.get(_).isEmpty)
            match
                case None => Some(pos)
                case Some(newPos) if (newPos.y == abyssY) => None
                case Some(newPos) => fallWithAbyss(grid, newPos, abyssY)
    }
    @tailrec
    def fallWithFloor(grid: Map[Coords, Char], pos: Coords, floorY: Int): Coords = {
        List(Coords(0, 1), Coords(-1, 1), Coords(1, 1))
            .map(pos + _)
            .find(grid.get(_).isEmpty)
            match
                case None => pos
                case Some(newPos) if (newPos.y == floorY - 1) => newPos
                case Some(newPos) => fallWithFloor(grid, newPos, floorY)
    }
    def initialize: Map[Coords, Char] = {
        Source
            .fromFile("input")
            .getLines
            .map(
                _
                .split(" -> ")
                .map(_.split(",") match
                        case Array(x, y) => Coords(x.toInt, y.toInt)
                )
                .foldLeft(List.empty[Coords])((L, point) =>
                    if (L.isEmpty) point::L
                    else {
                        val increment = Coords((L.head.x - point.x).sign, (L.head.y - point.y).sign)
                        (0 until L.head.distance(point)).map(point + increment * _).toList:::L
                    }
                )
                .map(_ -> '#')
            )
            .flatten
            .toMap
        + (source -> '+')
    }
}
