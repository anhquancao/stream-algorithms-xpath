package assignment2

import scala.collection.mutable
import scala.io.Source

object EagerDFA {

    // Stack storing the state
    private var S = mutable.Stack[String]()

    /**
      * Split query into array of tag
      * @param query
      * @return
      */
    def processQuery(query: String): Array[String] = {
        val tags = query.split("/")
        tags.slice(1, tags.length).map(x => if (x == "") "*" else x)
    }

    /**
      * Compute the next state base on
      * the DFA transitions and the current state
      * @param state
      * @param dfa
      * @param tag
      * @return
      */
    def eval(state: String, dfa: DFA, tag: String): String = {
        if (dfa.transitions.keys.exists(_ == (state, tag))) {
            dfa.transitions((state, tag))
        } else ""
    }

    def main(args: Array[String]): Unit = {
        val filename:String = args(0)
        val queryStr: String = args(1)

        // initial state at the document root
        var state: String = "0"

        val path = processQuery(queryStr)

        // build the NFA from the query
        val nfa = new NFA(path)

        // convert NFA to DFA
        val dfa: DFA = DFAConvert.convertEager(nfa)

        var nodeId = 0


        for (line <- Source.fromFile(filename).getLines) {

            // parse each line from the file
            val arr = line.split(" ")
            val bit = Integer.parseInt(arr(0))
            val tag = arr(1)


            if (bit == 0) {
                // start elemenent
                S.push(state)
                state = eval(state, dfa, tag)

                // check if the new state is in the Final state of DFA
                if (dfa.F.contains(state)) {
                    println(nodeId)
                }
                nodeId += 1
            } else {
                // Get the parent state
                state = S.pop()
            }

        }
    }

}
