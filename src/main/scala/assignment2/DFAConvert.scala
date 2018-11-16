package assignment2

import assignment2.EagerDFA.processQuery

object DFAConvert {
    def move(transition: Map[(Int, String), Int], states: Array[Int], a: String): Array[Int] = {
        var returnStates: Set[Int] = Set()
        for (s <- states) {
            if (transition.keys.exists(_ == (s, a))) {
                returnStates += transition((s, a))
            }
            if (transition.keys.exists(_ == (s, "*"))) {
                returnStates += s
            }
        }
        returnStates.toArray
    }

    def convertLazy(dfa: DFA, nfa: NFA, state: String, label: String): DFA = {

        var unmarkedStates: Array[String] = Array(state)
        dfa.F = dfa.F :+ nfa.F.toString

        var transitionLabels: Array[String] = (dfa.transitions.map(_._1._2).toArray :+ label).distinct

        while (unmarkedStates.length != 0) {
            val unmarkedState = unmarkedStates(0)
            for (label <- transitionLabels) {
                val unmarkedStateArray: Array[Int] = unmarkedState.split("").map(_.toInt)
                val states = move(nfa.transitions, unmarkedStateArray, label)

                if (states.length == 0) {
                    // transition to empty state
                    val deadState = ""
                    if (!dfa.Q.contains(deadState)) {
                        // add dead state
                        dfa.Q = dfa.Q :+ deadState
                        for (label <- transitionLabels) {
                            // dead state always go to itself
                            dfa.transitions += ((deadState, label) -> deadState)
                        }
                    }
                    dfa.transitions += ((unmarkedState, label) -> deadState)
                } else {
                    val newState = states.sorted.mkString("")
                    if (!dfa.Q.contains(newState)) {
                        unmarkedStates = unmarkedStates :+ newState
                        dfa.Q = dfa.Q :+ newState
                        if (states.contains(nfa.F)) {
                            dfa.F = dfa.F :+ newState
                        }
                    }
                    dfa.transitions += ((unmarkedState, label) -> newState)
                }
            }
            // mark state
            unmarkedStates = unmarkedStates.slice(1, unmarkedStates.length)
        }
        dfa
    }

    def convertEager(nfa: NFA): DFA = {
        val dfa = new DFA()
        dfa.Q = nfa.Q.map(_ + "")
        var unmarkedStates: Array[String] = dfa.Q
        dfa.F = dfa.F :+ nfa.F.toString

        val transitionLabels: List[String] = nfa.transitions.map(_._1._2).filter(_ != "*").toSet.toList

        while (unmarkedStates.length != 0) {
            val unmarkedState = unmarkedStates(0)
            for (label <- transitionLabels) {
                val unmarkedStateArray: Array[Int] = unmarkedState.split("").map(_.toInt)
                val states = move(nfa.transitions, unmarkedStateArray, label)

                if (states.length == 0) {
                    // transition to empty state
                    val deadState = ""
                    if (!dfa.Q.contains(deadState)) {
                        // add dead state
                        dfa.Q = dfa.Q :+ deadState
                        for (label <- transitionLabels) {
                            // dead state always go to itself
                            dfa.transitions += ((deadState, label) -> deadState)
                        }
                    }
                    dfa.transitions += ((unmarkedState, label) -> deadState)
                } else {
                    val newState = states.sorted.mkString("")
                    if (!dfa.Q.contains(newState)) {
                        unmarkedStates = unmarkedStates :+ newState
                        dfa.Q = dfa.Q :+ newState
                        if (states.contains(nfa.F)) {
                            dfa.F = dfa.F :+ newState
                        }
                    }
                    dfa.transitions += ((unmarkedState, label) -> newState)
                }
            }
            // mark state
            unmarkedStates = unmarkedStates.slice(1, unmarkedStates.length)
        }
        dfa
    }


    def main(args: Array[String]): Unit = {

        val queryStr: String = "//a/b//a"

        val path = processQuery(queryStr)

        val nfa = new NFA(path)

        val dfa = convertEager(nfa)

        val a = 2
    }

}
