import java.lang.Math.floorMod
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source


case class V2(x: Int, y: Int) {
    def +(other: V2) = V2(x + other.x, y + other.y)
    def -(other: V2) = V2(x - other.x, y - other.y)
    def isOnGrid(width: Int, height: Int) = ((0 until width) contains x) && ((0 until height) contains y)
    def isNeighbour(other: V2) = (this - other).x.abs + (this - other).y.abs == 1
}


object Main {
    val (width, height, initBlizzards) = init
    val period = lcm(width, height)
    val blizzards = precomputeBlizzards
    def main(args: Array[String]) = {
        val start = V2(0, -1)
        val end = V2(width - 1, height)
        val t1 = findMinTime(start, end).get
        println(t1)
        val t2 = findMinTime(end, start, t1).get
        val t3 = findMinTime(start, end, t1 + t2).get
        println(t1 + t2 + t3)
    }
    def init = {
        val lines = Source.fromFile("input").getLines.toVector
        val blizzards = for {
            (line, idx1) <- lines.zipWithIndex
            (c, idx2) <- line.zipWithIndex if (Seq('>', 'v', '<', '^') contains c)
        } yield (V2(idx2 - 1, idx1 - 1), c)
        (lines(0).length - 2, lines.length - 2, blizzards)
    }
    @tailrec
    def gcd(a: Int, b: Int): Int = {
        if (a == 0) b
        else if (b == 0) a
        else gcd(b, floorMod(a, b))
    }
    def lcm(a: Int, b: Int) = a * b / gcd(a, b)
    def precomputeBlizzards = {
        (0 until period)
            .scanLeft(initBlizzards)((curBlizzards, _) =>
                curBlizzards.map((pos, c) =>
                    val nextPos = c match
                        case '>' => V2(floorMod(pos.x + 1, width), pos.y)
                        case '<' => V2(floorMod(pos.x - 1, width), pos.y)
                        case '^' => V2(pos.x, floorMod(pos.y - 1, height))
                        case 'v' => V2(pos.x, floorMod(pos.y + 1, height))
                    (nextPos, c)
                )
            )
            .map(_.map(_._1).toSet)
    }
    def findMinTime(start: V2, end: V2, offset: Int = 0) = {
        @tailrec
        def bfs(nodes: Queue[(V2, Int)], visited: Set[(V2, Int)]): Option[Int] = {
            if (nodes.isEmpty) None
            else {
                val (position, time) = nodes.head
                if (position.isNeighbour(end)) {
                    Some(time + 1)
                } else {
                    val nextNodes = (Seq(Some(position), Some(start).filter(_.isNeighbour(position)))
                        ++ Seq(V2(1, 0), V2(0, 1), V2(-1, 0), V2(0, -1))
                            .map(move =>
                                Some(position + move)
                                    .filter(_.isOnGrid(width, height))
                            ))
                            .flatten
                            .filterNot(blizzards(floorMod(offset + time + 1, period)))
                            .filter(newPos => !visited((newPos, time + 1)))
                            .map((_, time + 1))
                    bfs(nodes.tail ++ nextNodes, visited ++ nextNodes)
                }
            }
        }
        bfs(Queue((start, 0)), Set((start, 0)))
    }
}
