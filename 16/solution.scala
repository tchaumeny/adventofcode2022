import scala.annotation.tailrec
import scala.io.Source


object Main {
    val valvesMap = initValvesMap
    val shortcutsMap = valvesMap
        .keys
        .map(valve => valve -> distanceMap(valve))
        .toMap
    def main(args: Array[String]) = {
        println(problem1)
        println(problem2)
    }
    def problem1 = maxRelease(30, "AA", valvesMap.filter(_._2._1 != 0).keySet)
    def problem2 = {
        val valves = valvesMap.filter(_._2._1 != 0).keySet
        valves
            .subsets
            .map(subset =>
                maxRelease(26, "AA", subset) + maxRelease(26, "AA", valves diff subset)
            )
            .max
    }
    def maxRelease(time: Int, from: String, valves: Set[String]): Int = {
        if (time == 0) 0
        else {
            valves
            .map(to => (to, shortcutsMap(from)(to)))
            .filter((to, steps) => steps + 1 <= time)
            .map((to, steps) =>
                (time - steps - 1) * valvesMap(to)._1 + maxRelease(time - steps - 1, to, valves - to)
            )
            .maxOption
            .getOrElse(0)
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
