package assignment2


class NFA(path: Array[String]) {
    val I = 0
    var transitions: Map[(Int, String), Int] = Map()
    var Q: Array[Int] = Array(0)

    var i = 0
    for (p <- path) {
        if (p == "*") {
            transitions += ((i, p) -> i)
        } else {
            transitions += ((i, p) -> (i + 1))
            i += 1
            Q = Q :+ i
        }

    }

    val F: Int = Q.last

}
