package assignment2

import scala.collection.mutable
import scala.io.Source

object Main {

    private var S = mutable.Stack[Array[Int]]()

    def processQuery(query: String): Array[String] = {
        val tags = query.split("/")
        tags.slice(1, tags.length).map(x => if (x == "") "*" else x)
    }

    def eval(state: Array[Int], nfa: NFA, tag: String): Array[Int] = {
        var newState: Array[Int] = state

        for (s <- state) {
            if (nfa.transitions.keys.exists(_ == (s, tag))) newState = newState :+ nfa.transitions((s, tag))
        }

        newState
    }

    def main(args: Array[String]): Unit = {
        val filename: String = "data/input.txt"
        val queryStr: String = "//a/b//a"

        var state: Array[Int] = Array(0)

        val path = processQuery(queryStr)

        val nfa = new NFA(path)

        var nodeId = 0


        for (line <- Source.fromFile(filename).getLines) {
            val arr = line.split(" ")
            val bit = Integer.parseInt(arr(0))
            val tag = arr(1)


            if (bit == 0) {
                // start elemenent
                S.push(state)
                state = eval(state, nfa, tag)
                if (state.contains(nfa.F)) {
                    println(nodeId)
                }
                nodeId += 1
            } else {
                state = S.pop()
            }

        }
    }

}
