import scala.io.Source
import scala.collection.mutable.ListBuffer


trait TreeNode {
    val name: String
    def print(indent: Int): Unit
    def size: Int
}
case class Directory(name: String, children: ListBuffer[TreeNode], parent: Option[Directory] = None) extends TreeNode {
    def print(indent: Int): Unit = {
        println(s"${" " * indent}- $name (dir)")
        children.foreach(_.print(indent + 2))
    }
    def size = children.map(_.size).sum
    def listDirs: Iterable[Directory] = {
        this +: children.flatMap{
            case directory: Directory => directory.listDirs
            case _ => Nil
        }
    }
}
case class File(name: String, size: Int) extends TreeNode {
    def print(indent: Int): Unit = {
        println(s"${" " * indent}- $name (file, size=$size)")
    }
}


object Main {
    def main(args: Array[String]) = {
        val root = parseDirectory(Source.fromFile("input").getLines)
        val problem1 = root.listDirs.map(_.size).filter(_ < 100000).sum
        println(s"The sum of total sizes of the matching directories is $problem1")

        val freeUpSpace = 30000000 - (70000000 - root.size)
        val problem2 = root.listDirs.map(_.size).filter(_ >= freeUpSpace).min
        println(s"The size of the directory to delete is $problem2")
    }
    def parseDirectory(commands: Iterator[String]): Directory = {
        val cdPattern = "\\$ cd ([\\w.]+)".r
        val dirPattern = "dir (\\w+)".r
        val filePattern = "(\\d+) ([\\w.]+)".r

        val root = Directory("/", ListBuffer())
        commands
            .drop(1)
            .foldLeft(root)((currentDir: Directory, instruction: String) =>
                instruction match
                    case cdPattern("..") => currentDir.parent.get
                    case cdPattern(dirName) =>
                        currentDir.children.find(_.name == dirName).get.asInstanceOf[Directory]
                    case dirPattern(dirName) => {
                        currentDir.children += Directory(dirName, ListBuffer(), Some(currentDir))
                        currentDir
                    }
                    case filePattern(size, fileName) => {
                        currentDir.children += File(fileName, size.toInt)
                        currentDir
                    }
                    case _ => currentDir    // ls are ignored
            )
        root
    }
}
