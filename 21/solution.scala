import scala.io.Source


sealed trait Expression {
    def evaluate(expressions: Map[String, Expression], unknown: Option[BigDecimal] = None): BigDecimal
}

case class Value(value: BigDecimal) extends Expression {
    def evaluate(expressions: Map[String, Expression], unknown: Option[BigDecimal] = None): BigDecimal = value
}

case class Unknown(default: BigDecimal) extends Expression {
    def evaluate(expressions: Map[String, Expression], unknown: Option[BigDecimal] = None): BigDecimal = unknown.getOrElse(default)
}

case class Operation(lhs: String, rhs: String, op: Char) extends Expression {
    def evaluate(expressions: Map[String, Expression], unknown: Option[BigDecimal] = None): BigDecimal = {
        val lvalue = expressions(lhs).evaluate(expressions, unknown)
        val rvalue = expressions(rhs).evaluate(expressions, unknown)
        op match
            case '+' => lvalue + rvalue
            case '-' => lvalue - rvalue
            case '*' => lvalue * rvalue
            case '/' => lvalue / rvalue
    }
    def compare(expressions: Map[String, Expression], unknown: BigDecimal): BigDecimal = {
        val lvalue = expressions(lhs).evaluate(expressions, Some(unknown))
        val rvalue = expressions(rhs).evaluate(expressions, Some(unknown))
        (rvalue - lvalue).sign
    }
}


object Main {
    val operationPattern = """(\w+): (\w+) ([+\-*/]) (\w+)""".r
    val valuePattern = """(\w+): (\d+)""".r
    val expressions = Source
        .fromFile("input")
        .getLines
        .map{
            case operationPattern(id, lhs, op, rhs) => id -> Operation(lhs, rhs, op.charAt(0))
            case valuePattern("humn", value) => "humn" -> Unknown(BigDecimal(value))
            case valuePattern(id, value) => id -> Value(BigDecimal(value))
        }
        .toMap
    def main(args: Array[String]) = {
        println(problem1)
        println(problem2)
    }
    def problem1 = expressions("root").evaluate(expressions)
    def problem2 = {
        val root = expressions("root").asInstanceOf[Operation]
        findZero(root, -1_000_000_000_000_000L, 1_000_000_000_000_000L).get
    }
    @scala.annotation.tailrec
    def findZero(root: Operation, a: BigInt, b: BigInt): Option[BigInt] = {
        // We assume the root value if a monotonic function of
        // the variable humn value, which is not true in the general case
        val middle = (a + b) / 2
        val middleSign = root.compare(expressions, BigDecimal(middle))
        if (middleSign == 0) Some(middle)
        else {
            val aSign = root.compare(expressions, BigDecimal(a))
            val bSign = root.compare(expressions, BigDecimal(b))
            if (middleSign != aSign) findZero(root, a, middle - 1)
            else if (middleSign != bSign) findZero(root, middle + 1, b)
            else None
        }
    }
}
