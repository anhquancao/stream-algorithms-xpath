package assignment2

import scala.io.Source

object Main {

    def processQuery(query: String): Array[String] = {
        val tags = query.split("/")
        tags.slice(1, tags.length).map(x => if (x == "") "*" else x)
    }

    def main(args: Array[String]): Unit = {
        val filename: String = "data/input.txt"
        val queryStr: String = "//a/b//a"

        val path = processQuery(queryStr)

        val nfa = new NFA(path)

        var nodeId = 0

        for (line <- Source.fromFile(filename).getLines) {
            val arr = line.split(" ")
            val bit = Integer.parseInt(arr(0))
            val tag = arr(1)

            if (bit == 0) {
                //                startElement(tag, path, nodeId)
                nodeId += 1
            } else {
                //                endElement(tag)
            }

        }
    }

}
