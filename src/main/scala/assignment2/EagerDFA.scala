package assignment2

import scala.collection.mutable
import scala.io.Source

object EagerDFA {

    private var S = mutable.Stack[String]()

    def processQuery(query: String): Array[String] = {
        val tags = query.split("/")
        tags.slice(1, tags.length).map(x => if (x == "") "*" else x)
    }

    def eval(state: String, dfa: DFA, tag: String): String = {
        if (dfa.transitions.keys.exists(_ == (state, tag))) {
            dfa.transitions((state, tag))
        } else ""
    }

    def main(args: Array[String]): Unit = {
        val filename:String = args(0)
        val queryStr: String = args(1)

        var state: String = "0"

        val path = processQuery(queryStr)

        val nfa = new NFA(path)

        val dfa: DFA = DFAConvert.convertEager(nfa)

        var nodeId = 0


        for (line <- Source.fromFile(filename).getLines) {
            val arr = line.split(" ")
            val bit = Integer.parseInt(arr(0))
            val tag = arr(1)


            if (bit == 0) {
                // start elemenent
                S.push(state)
                state = eval(state, dfa, tag)
                if (dfa.F.contains(state)) {
                    println(nodeId)
                }
                nodeId += 1
            } else {
                state = S.pop()
            }

        }
    }

}
