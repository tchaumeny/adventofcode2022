import scala.annotation.tailrec
import scala.io.Source
import scala.collection.mutable.Buffer


case class Coords(x: Int, y: Int) {
    def distance(other: Coords) = (x - other.x).abs + (y - other.y).abs
}


def ints(s: String) = "-?\\d+".r.findAllIn(s).map(_.toInt)


type Interval = (Int, Int)


@tailrec
def mergeIntervals(intervals: Seq[Interval], offset: Int = 0): Seq[Interval] = {
    if (intervals.length - offset <= 1) intervals
    else if (intervals(offset + 1)._1 <= intervals(offset)._2 + 1) {
        val merged = (intervals(offset)._1, intervals(offset)._2.max(intervals(offset + 1)._2))
        mergeIntervals((intervals.slice(0, offset) :+ merged) ++ intervals.drop(offset + 2), offset)
    } else mergeIntervals(intervals, offset + 1)
}


object Main {
    val pairs = Source
        .fromFile("input")
        .getLines()
        .map(ints(_).toSeq match
            case Seq(sx, sy, bx, by) => (Coords(sx, sy), Coords(bx, by))
        )
        .toVector
    def main(args: Array[String]) = {
        println(s"There are ${analyseRow(2_000_000)} positions that cannot contain a beacon.")
        println(s"The tuning frequency is ${tuningFrequency(4_000_000)}")
    }
    def analyseRow(y: Int) = {
        pairs
            .flatMap((sensor, beacon) =>
                val maxXDistance = sensor.distance(beacon) - (sensor.y - y).abs
                (-maxXDistance to maxXDistance)
                    .map(x => Coords(sensor.x + x, y))
                    .filter(_ != beacon)
                    .map(_.x)
            )
            .distinct
            .size
    }
    def tuningFrequency(sideLength: Int) = {
        (0 to sideLength).map(y =>
            val intervals = pairs
                .map((sensor, beacon) =>
                    val maxXDistance = sensor.distance(beacon) - (sensor.y - y).abs
                    (sensor.x - maxXDistance, sensor.x + maxXDistance)
                )
                .filter(_ <= _)
                .sortBy(_._1)
            mergeIntervals(intervals)
        )
        .zipWithIndex
        .find(_._1.length != 1)
        .map((intervals, y) =>
            BigInt(4_000_000) * (intervals.head._2 + 1) + y
        )
        .get
    }
}
