import scala.io.Source


object Main {
    def main(args: Array[String]) = {
        for (markerLength <- List(4, 14)) {
            val count = 
            Source
                .fromFile("input")
                .mkString
                .sliding(markerLength, 1)
                .zipWithIndex
                .find(_._1.distinct.size == markerLength)
                .map(_._2 + markerLength)
                .get
            println(s"The first marker of size $markerLength is found after $count chars")
        }
    }
}
