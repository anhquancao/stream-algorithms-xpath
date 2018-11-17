package assignment1


import scala.collection.mutable
import scala.io.Source

object SimpleXPath {

    // Stack storing the matching position
    private var S = mutable.Stack[Int]()

    // Storing a list of previous with the size = query size
    private var previousNodes: Array[String] = Array()

    // set the current maching position
    private var currentStep = 0

    /**
      * Find all the prefixes of the queries using tail recursive.
      * @param queries
      * @return
      */
    def findPrefixes(queries: Array[String]): Array[String] = {
        def findPrefixesRecur(queries: Array[String], res: Array[String]): Array[String] = {
            if (queries.length == 0) res
            else findPrefixesRecur(queries.dropRight(1), res :+ queries.mkString(""))
        }

        findPrefixesRecur(queries.dropRight(1), Array())
    }
    /**
      * Find all the posfixes of the queries using tail recursive.
      * @param queries
      * @return
      */
    def findPosfixes(queries: Array[String]): Array[String] = {
        def findPosfixesRecur(queries: Array[String], res: Array[String]): Array[String] = {
            if (queries.length == 0) res
            else findPosfixesRecur(queries.drop(1), res :+ queries.mkString(""))
        }

        findPosfixesRecur(queries.drop(1), Array())
    }

    /**
      * KMP algorithm to find the longest match of
      * the prefix of queryPath and the posfix of the nodePath
      * @param nodePath
      * @param queryPath
      * @return
      */
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

    /**
      * Split query into array of tag
      * @param query
      * @return
      */
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
            if (currentStep == path.length - 1) {
                // found a match
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
        // get the parent matching position
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
