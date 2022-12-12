import scala.io.Source


type Point = (Int, Int)

class HeightMap(private val grid: Vector[Vector[Char]]) {
    def points: Vector[(Point, Char)] =
        for {
            (row, i) <- grid.zipWithIndex
            (elevation, j) <- row.zipWithIndex
        } yield ((i, j), elevation)
    def startPoint: Point = points.find(_._2 == 'S').map(_._1).head
    def endPoint: Point = points.find(_._2 == 'E').map(_._1).head
    def elevation(point: Point): Char = grid(point._1)(point._2) match
        case 'S' => 'a'
        case 'E' => 'z'
        case other => other
}


object Main {
    val heightMap = HeightMap(
        Source
        .fromFile("input")
        .getLines
        .map(_.toVector)
        .toVector
    )

    def main(args: Array[String]) = {
        val distances = dijkstraDistances(heightMap, heightMap.endPoint)
        println(s"The fewest steps required from start to end is ${distances(heightMap.startPoint).toInt}")

        val problem2 = distances.view.filterKeys(heightMap.elevation(_) == 'a').values.min.toInt
        println(s"The fewest steps required from a square at elevation 'a' is $problem2")
    }

    def dijkstraDistances(heightMap: HeightMap, target: Point): Map[Point, Double] = {
        val Q = collection.mutable.Set[Point]()
        val distances = collection.mutable.Map[Point, Double]()

        heightMap
            .points
            .map(_._1)
            .foreach(point =>
                Q.add(point)
                distances(point) = Double.PositiveInfinity
            )
        distances(target) = 0

        while !Q.isEmpty do {
            val point = Q.minBy(distances)
            Q.remove(point)

            List((-1, 0), (1, 0), (0, -1), (0, 1))
                .map(move => (point._1 + move._1, point._2 + move._2))
                .filter(Q.contains)
                .filter(heightMap.elevation(_) >= heightMap.elevation(point) - 1)
                .foreach(neighbour =>
                    if (distances(point) + 1 < distances(neighbour))
                        distances(neighbour) = distances(point) + 1
                )
        }
        distances.toMap
    }
}
