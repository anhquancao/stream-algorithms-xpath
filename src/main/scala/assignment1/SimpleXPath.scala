package assignment1


import scala.collection.mutable
import scala.io.Source

object SimpleXPath {
    private var S = mutable.Stack[Int]()

    private var previousNodes: Array[String] = Array()

    private var currentStep = 0

    def findPrefixes(queries: Array[String]): Array[String] = {
        def findPrefixesRecur(queries: Array[String], res: Array[String]): Array[String] = {
            if (queries.length == 0) res
            else findPrefixesRecur(queries.dropRight(1), res :+ queries.mkString(""))
        }

        findPrefixesRecur(queries.dropRight(1), Array())
    }

    def findPosfixes(queries: Array[String]): Array[String] = {
        def findPosfixesRecur(queries: Array[String], res: Array[String]): Array[String] = {
            if (queries.length == 0) res
            else findPosfixesRecur(queries.drop(1), res :+ queries.mkString(""))
        }

        findPosfixesRecur(queries.drop(1), Array())
    }


    def KMP(nodePath: Array[String], queryPath: Array[String]): Int = {

        var lengths: Array[Int] = Array()
        val prefixes = findPrefixes(queryPath)
        val posfixes = findPosfixes(nodePath)
        for (prefix <- prefixes) {
            for (posfix <- posfixes) {
                if (prefix.equalsIgnoreCase(posfix)) lengths = lengths :+ prefix.length
            }
        }
        if (lengths.length > 0)
            lengths.max
        else 0
    }

    def processQuery(query: String): Array[String] = {
        val tags = query.split("/")
        tags.slice(2, tags.length)
    }

    def startElement(tag: String, path: Array[String], nodeId: Int): Unit = {
        S.push(currentStep)

        // Store previous nodes
        // previousNodes has the same size of the query path
        if (previousNodes.length == path.length) {
            previousNodes = previousNodes.slice(1, previousNodes.length)
        }
        previousNodes = previousNodes :+ tag

        if (tag == path(currentStep)) {
            // match
            if (currentStep == path.length - 1) {
                println(nodeId)
                currentStep = KMP(previousNodes, path)
            } else {
                currentStep += 1
            }
        } else {
            // Failure transition
            currentStep = KMP(previousNodes, path)
        }
    }

    def endElement(tag: String): Unit = {
        currentStep = S.pop()
    }

    def main(args: Array[String]): Unit = {
        val filename:String = args(0)
        val queryStr: String = args(1)

        val path = processQuery(queryStr)

        var nodeId = 0

        for (line <- Source.fromFile(filename).getLines) {
            val arr = line.split(" ")
            val bit = Integer.parseInt(arr(0))
            val tag = arr(1)

            if (bit == 0) {
                startElement(tag, path, nodeId)
                nodeId += 1
            } else {
                endElement(tag)
            }

        }
    }

}
