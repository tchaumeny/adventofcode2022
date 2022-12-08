import scala.io.Source


object Main {
    val treesMatrix = Source
        .fromFile("input")
        .getLines
        .map(_
            .map(_.toInt)
            .toVector
        )
        .toVector

    val (width, height) = (treesMatrix.length, treesMatrix(0).length)

    case class Tree(i: Int, j: Int) {
        def >=(other: Tree) = treesMatrix(i)(j) >= treesMatrix(other.i)(other.j)
    }

    def main(args: Array[String]) = {
        println(s"There are $problem1 visible trees")
        println(s"The highest scenic score is $problem2")
    }
    def problem1 = {
        List( // Seen from Left, Right, Top, Bottom
            for i <- 0 until height yield for j <- 0 until width yield Tree(i, j),
            for i <- 0 until height yield for j <- width - 1 to 0 by -1 yield Tree(i, j),
            for j <- 0 until width yield for i <- 0 until height yield Tree(i, j),
            for j <- 0 until width yield for i <- height - 1 to 0 by -1 yield Tree(i, j),
        )
        .flatten
        .map(
            _.foldLeft(Nil)((visible: List[Tree], current: Tree) =>
                visible.headOption
                       .filter(_ >= current)
                       .map(_ => visible)
                       .getOrElse(current::visible)
            )
        )
        .flatten
        .distinct
        .length
    }
    def problem2 = {
        (for i <- 0 until width; j <- 0 until height yield {
            Array(  // Up, Down, Left, Right
                (i - 1 to 0 by -1).map(Tree(_, j)),
                (i + 1 until height).map(Tree(_, j)),
                (j - 1 to 0 by -1).map(Tree(i, _)),
                (j + 1 until width).map(Tree(i, _)),
            ).map(trees =>
                trees
                .zipWithIndex
                .find(_._1 >= Tree(i, j))
                .map(_._2 + 1)
                .getOrElse(trees.length)
            ).reduce(_ * _)
        }).max
    }
}
