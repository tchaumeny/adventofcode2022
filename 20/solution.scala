import scala.io.Source


class DLLNode(val value: BigInt, var previous: Option[DLLNode] = None, var next: Option[DLLNode] = None) {
    def insertRight(value: BigInt): DLLNode = {
        val newNode = DLLNode(value)
        newNode.previous = Some(this)
        newNode.next = next
        next.get.previous = Some(newNode)
        next = Some(newNode)
        newNode
    }
    def remove: Unit = {
        previous.get.next = next
        next.get.previous = previous
    }
}


type NodesState = collection.mutable.Map[Int, DLLNode]


object Main {
    val initValues = Source
        .fromFile("input")
        .getLines
        .map(BigInt(_))
        .toList
    def main(args: Array[String]) = {
        println(problem1)
        println(problem2)
    }
    def problem1 = {
        val nodes = buildNodes(initValues)
        mixNodes(initValues, nodes)
        groveNumber(nodes)
    }
    def problem2 = {
        val values = initValues.map(_ * 811589153)
        val nodes = buildNodes(values)
        (1 to 10).foreach(_ => mixNodes(values, nodes))
        groveNumber(nodes)
    }
    def buildNodes(values: List[BigInt]): NodesState = {
        var first: Option[DLLNode] = None
        var last: Option[DLLNode] = None
        val nodes: collection.mutable.Map[Int, DLLNode] = collection.mutable.Map.empty

        for ((value, idx) <- values.zipWithIndex) {
            val node = Some(DLLNode(value))
            nodes(idx) = node.get
            if (first.isEmpty) {
                first = node
            } else {
                last.get.next = node
                node.get.previous = last
            }
            last = node
        }
        last.get.next = first
        first.get.previous = last
        nodes
    }
    def mixNodes(values: List[BigInt], nodes: NodesState) = {
        for ((value, idx) <- values.zipWithIndex) {
            val steps = (value % (values.length - 1)).toInt
            if (steps != 0) {
                val initNode = nodes(idx)
                var curNode = initNode
                initNode.remove
                if (steps > 0) {
                    for (step <- 1 to steps) {
                        curNode = curNode.next.get
                    }
                    nodes(idx) = curNode.insertRight(initNode.value)
                } else {
                    for (step <- 1 to -steps) {
                        curNode = curNode.previous.get
                    }
                    nodes(idx) = curNode.previous.get.insertRight(initNode.value)
                }
            }
        }
    }
    def groveNumber(nodes: NodesState) = {
        val zero = nodes.values.find(_.value == 0).get
        @scala.annotation.tailrec
        def func(head: DLLNode, offset: Int, acc: BigInt): BigInt = {
            offset match
                case 3000 => acc + head.value
                case 1000 | 2000 => func(head.next.get, offset + 1, acc + head.value)
                case _ => func(head.next.get, offset + 1, acc)
        }
        func(zero, 0, 0)
    }
}
