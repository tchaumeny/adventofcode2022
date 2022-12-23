import java.lang.Math.floorMod
import scala.annotation.tailrec
import scala.io.Source


case class V2(x: Int, y: Int) {
    def +(other: V2) = V2(x + other.x, y + other.y)
}

val directions = Seq(V2(0, -1), V2(0, 1), V2(1, 0), V2(-1, 0), V2(1, -1), V2(-1, -1), V2(1, 1), V2(-1, 1))
val Seq(n, s, e, w, ne, nw, se, sw) = directions
val strategies = Seq(
    Seq(n, ne, nw),
    Seq(s, se, sw),
    Seq(w, nw, sw),
    Seq(e, ne, se),
)


object Main {
    def main(args: Array[String]) = {
        println(problem1)
        println(problem2)
    }
    def problem1 = {
        val elves = run(init, 10)
        val minX = elves.map(_.x).min
        val maxX = elves.map(_.x).max
        val minY = elves.map(_.y).min
        val maxY = elves.map(_.y).max

        (for {
            x <- minX to maxX
            y <- minY to maxY
        } yield V2(x, y))
        .filterNot(elves)
        .length
    }
    def problem2 = findStable(init)
    def init = {
        Source
            .fromFile("input")
            .getLines
            .zipWithIndex
            .flatMap((s, y) =>
                s
                .zipWithIndex
                .flatMap((c, x) =>
                    Some(V2(x, y)).filter(_ => c == '#')
                )
            )
            .toSet
    }
    @tailrec
    def run(elves: Set[V2], rounds: Int, offset: Int = 0): Set[V2] = {
        if (rounds == 0) elves
        else run(moveOnce(elves, offset), rounds - 1, offset + 1)
    }
    @tailrec
    def findStable(elves: Set[V2], offset: Int = 0): Int = {
        val newElves = moveOnce(elves, offset)
        if (newElves == elves) offset + 1
        else findStable(newElves, offset + 1)
    }
    def moveOnce(elves: Set[V2], offset: Int): Set[V2] = {
        val propositions = elves
            .toSeq
            .flatMap(coords =>
                if (directions.map(coords + _).exists(elves)) {
                    (0 to strategies.length - 1)
                        .map(idx => strategies(floorMod(idx + offset, strategies.length)))
                        .find(strategy => !strategy.exists(move => elves(coords + move)))
                        .map(strategy => coords -> (coords + strategy.head))
                } else {
                    None
                }
            )
        val collisions = propositions
            .groupBy(_._2)
            .view
            .mapValues(_.length)
            .filter(_._2 > 1)
            .keySet
        propositions
            .filterNot(proposition => collisions(proposition._2))
            .foldLeft(elves){case (elves, (moveFrom, moveTo)) =>
                elves - moveFrom + moveTo
            }
    }
}
