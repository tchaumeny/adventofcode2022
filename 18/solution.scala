import scala.io.Source


case class Coords(x: Int, y: Int, z: Int) {
    def +(other: Coords) = Coords(x + other.x, y + other.y, z + other.z)
}


object Main {
    val directions = List(
        Coords(1, 0, 0), Coords(-1, 0, 0),
        Coords(0, 1, 0), Coords(0, -1, 0),
        Coords(0, 0, 1), Coords(0, 0, -1),
    )
    val cubes = Source
        .fromFile("input")
        .getLines
        .map(_.split(",") match
            case Array(x, y, z) => Coords(x.toInt, y.toInt, z.toInt)
        )
        .toSet
    val minX = cubes.map(_.x).min
    val minY = cubes.map(_.y).min
    val minZ = cubes.map(_.z).min
    val maxX = cubes.map(_.x).max
    val maxY = cubes.map(_.y).max
    val maxZ = cubes.map(_.z).max
    def main(args: Array[String]) = {
        println(problem1)
        println(problem2)
    }
    def problem1 = {
        cubes
            .toSeq
            .flatMap(cube =>
                directions
                    .map(cube + _)
                    .filter(!cubes(_))
            )
            .length
    }
    def problem2 = {
        val exterior = exteriorSet
        cubes
            .toSeq
            .flatMap(cube =>
                directions
                    .map(cube + _)
                    .filter(cube =>
                        !isWithinMinMaxCube(cube)
                        || exterior(cube)
                    )
            )
            .length
    }

    def isWithinMinMaxCube(cube: Coords): Boolean = {
        if !(minX to maxX contains cube.x) then false
        else if !(minY to maxY contains cube.y) then false
        else if !(minZ to maxZ contains cube.z) then false
        else true
    }

    def exteriorSet: Set[Coords] = {
        val minXface = for (y <- -minY to maxY; z <- -minZ to maxZ) yield Coords(minX, y, z)
        val maxXface = for (y <- -minY to maxY; z <- -minZ to maxZ) yield Coords(maxX, y, z)
        val minYFace = for (x <- -minX to maxX; z <- -minZ to maxZ) yield Coords(x, minY, z)
        val maxYface = for (x <- -minX to maxX; z <- -minZ to maxZ) yield Coords(x, maxY, z)
        val minZFace = for (x <- -minX to maxX; y <- -minY to maxY) yield Coords(x, y, minZ)
        val maxZFace = for (x <- -minX to maxX; y <- -minY to maxY) yield Coords(x, y, maxZ)

        val exterior = (minXface ++ maxXface ++ minYFace ++ maxYface ++ minZFace ++ maxZFace)
            .filter(!cubes(_))

        @scala.annotation.tailrec
        def growExterior(exterior: Set[Coords], unexplored: Seq[Coords]): Set[Coords] = {
            if (unexplored.isEmpty) exterior
            else {
                val neighbours = unexplored
                    .flatMap(air =>
                        directions
                            .map(air + _)
                            .filter(isWithinMinMaxCube)
                            .filter(!exterior(_))
                            .filter(!cubes(_))
                    )
                growExterior(exterior ++ neighbours, neighbours)
            }
        }
        growExterior(exterior.toSet, exterior)
    }
}
