package assignment2

import scala.collection.mutable
import scala.io.Source

object LazyDFA {

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


    def main(args: Array[String]): Unit = {
        val filename:String = args(0)
        val queryStr: String = args(1)

        // initial state at the document root
        var state: String = "0"

        val path = processQuery(queryStr)

        // build the NFA from the query
        val nfa = new NFA(path)

        var nodeId = 0

        // Create empty DFA
        var dfa: DFA = new DFA()


        for (line <- Source.fromFile(filename).getLines) {
            val arr = line.split(" ")
            val bit = Integer.parseInt(arr(0))
            val tag = arr(1)


            if (bit == 0) {
                // start elemenent
                S.push(state)

                // Extend the DFA using the current state and tag
                dfa = DFAConvert.convertLazy(dfa, nfa, state, tag)

                // using the DFA to find the next state
                if (dfa.transitions.keys.exists(_ == (state, tag))) {
                    state = dfa.transitions((state, tag))
                } else {
                    state = ""
                }

                // check if the new state in the Final states of the DFA
                if (dfa.F.contains(state)) {
                    println(nodeId)
                }
                nodeId += 1
            } else {
                // get the parent state
                state = S.pop()
            }

        }
    }

}
