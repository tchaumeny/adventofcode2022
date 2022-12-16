import scala.annotation.tailrec
import scala.io.Source


object Main {
    val valvesMap = initValvesMap
    val shortcutsMap = valvesMap
        .keys
        .map(valve => valve -> distanceMap(valve))
        .toMap
    def main(args: Array[String]) = {
        //println(valvesMap)
        //shortcutsMap
        //    .foreach((valve, shortcuts) =>
        //        println(s"$valve:")
        //        println(shortcuts)
        //    )
        println(maxRelease(30, "AA"))
    }
    def maxRelease(time: Int, from: String, openValves: Set[String] = Set.empty): Int = {
        if (time == 0) 0
        else {
            val releasePerStep = openValves.map(valvesMap(_)._1).sum
            shortcutsMap(from)
                .filter((to, steps) => steps + 1 <= time && !openValves(to) && valvesMap(to)._1 != 0)
                .map((to, steps) =>
                    (steps + 1) * releasePerStep + maxRelease(time - steps - 1, to, openValves + to)
                )
                .maxOption
                .getOrElse(time * releasePerStep)
        }
    }
    def initValvesMap = {
        val valvePattern = """Valve ([A-Z]+) has flow rate=(\d+); tunnels? leads? to valves? ([A-Z,\s]+)""".r
        Source
            .fromFile("input")
            .getLines
            .map{ case valvePattern(id, flow, links) =>
                id -> (flow.toInt, links.split(", ").toList)
            }
            .toMap
    }

    def distanceMap(source: String) = {
        @tailrec
        def integrateBatch(batch: List[String], distance: Int, distances: Map[String, Int], seen: Set[String]): Map[String, Int] = {
            if (batch.isEmpty) distances
            else {
                val newBatch = batch.flatMap(valve => valvesMap(valve)._2).filterNot(seen)
                integrateBatch(newBatch, distance + 1, distances ++ newBatch.map(_ -> distance), seen ++ newBatch)
            }
        }
        integrateBatch(List(source), 1, Map.empty, Set(source))
    }
}
