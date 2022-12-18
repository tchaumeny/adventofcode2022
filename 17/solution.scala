import java.lang.Math.floorMod
import scala.annotation.tailrec
import scala.io.Source


case class Coords(x: Int, y: Int) {
    def +(other: Coords) = Coords(x + other.x, y + other.y)
}

val shapes = Seq(
    Seq(Coords(0, 0), Coords(1, 0), Coords(2, 0), Coords(3, 0)),
    Seq(Coords(1, 0), Coords(0, 1), Coords(1, 1), Coords(2, 1), Coords(1, 2)),
    Seq(Coords(0, 0), Coords(1, 0), Coords(2, 0), Coords(2, 1), Coords(2, 2)),
    Seq(Coords(0, 0), Coords(0, 1), Coords(0, 2), Coords(0, 3)),
    Seq(Coords(0, 0), Coords(1, 0), Coords(0, 1), Coords(1, 1)),
)
val jets = Source.fromFile("input").mkString.trim

type StateSignature = (Int, Int, String)
val signHeight = 30

case class State(rockCnt: Int, jetCnt: Int, frozenRock: Set[Coords]) {
    def stackHeight = frozenRock.map(_.y + 1).maxOption.getOrElse(0)
    def shape = shapes(floorMod(rockCnt, shapes.length))
    def jet = jets(floorMod(jetCnt, jets.length))
    def signature: Option[StateSignature] = {
        val height = stackHeight
        if (height < signHeight) None
        else {
            val rocksSign = (height until height - 30 by -1)
                .map(y =>
                    (0 until 7)
                        .map(x => if (frozenRock(Coords(x, y))) 2 << x else 0)
                        .sum
                        .toChar
                )
                .mkString
            Some((floorMod(rockCnt, shapes.length), floorMod(jetCnt, jets.length), rocksSign))
        }
    }
}

type Cycle = (Int, Int, Int)


object Main {
    def main(args: Array[String]) = {
        println(problem1)
        println(problem2)
    }

    def problem1 = simulateRocks(2022).stackHeight

    def problem2 = {
        val N = 1_000_000_000_000L
        val (firstAt, secondAt, cycleHeight) = findCycle()
        val period = secondAt - firstAt
        simulateRocks(firstAt + floorMod(N - firstAt, period).toInt).stackHeight + (N - firstAt) / period * cycleHeight
    }

    @tailrec
    def findCycle(state: State = State(0, 0, Set.empty), seenStates: Map[StateSignature, (Int, Int)] = Map.empty): Cycle = {
        val newState = rockFall(state, Coords(2, state.stackHeight + 3))
        newState.signature.flatMap(seenStates.get) match
            case Some((firstCnt, firstHeight)) =>
                (firstCnt, newState.rockCnt, newState.stackHeight - firstHeight)
            case None =>
                findCycle(
                    newState,
                    newState.signature.map(signature =>
                        seenStates + (signature -> (newState.rockCnt, newState.stackHeight))
                    ).getOrElse(seenStates)
                )
    }

    @tailrec
    def simulateRocks(cnt: Int, state: State = State(0, 0, Set.empty)): State = {
        if (cnt == 0) state
        else {
            simulateRocks(
                cnt - 1,
                rockFall(state, Coords(2, state.stackHeight + 3)),
            )
        }
    }

    @tailrec
    def rockFall(state: State, position: Coords): State = {
        val afterJetPos = if (
            state.jet == '>'
            && position.x + state.shape.map(_.x).max < 6
            && !state
                .shape
                .map(_ + position + Coords(1, 0))
                .exists(state.frozenRock)
        ) {
            position + Coords(1, 0)
        } else if (
            state.jet == '<'
            && position.x > 0
            && !state
                .shape
                .map(_ + position + Coords(-1, 0))
                .exists(state.frozenRock)
        ) {
            position + Coords(-1, 0)
        } else position
        if (
            afterJetPos.y == 0
            || state
                .shape
                .map(_ + afterJetPos + Coords(0, -1))
                .exists(state.frozenRock)
        ) {
            State(
                state.rockCnt + 1,
                state.jetCnt + 1,
                state.frozenRock ++ state.shape.map(_ + afterJetPos)
            )
        } else {
            rockFall(
                State(state.rockCnt, state.jetCnt + 1, state.frozenRock),
                afterJetPos + Coords(0, -1)
            )
        }
    }
}
