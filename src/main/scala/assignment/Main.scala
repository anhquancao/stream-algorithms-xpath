package assignment

import scala.io.Source

object Main {

    var index = 0


    def startElement(tag: String) = {

    }

    def endElement(tag: String): Unit = {

    }

    def findPrefixes(queries: Array[String]) = {
        def findPrefixesRecur(queries: Array[String], res: Array[String]): Array[String] = {
            if (queries.length == 0) res
            else findPrefixesRecur(queries.dropRight(1), res :+ queries.mkString(""))
        }

        findPrefixesRecur(queries.dropRight(1), Array())
    }

    def findPosfixes(queries: Array[String]) = {
        def findPosfixesRecur(queries: Array[String], res: Array[String]): Array[String] = {
            if (queries.length == 0) res
            else findPosfixesRecur(queries.drop(1), res :+ queries.mkString(""))
        }
        findPosfixesRecur(queries.drop(1), Array())
    }


    def KMP(queries: Array[String]): Unit = {

        val prefixes = findPrefixes(queries)
        val posfixes = findPosfixes(queries)
        val matches = prefixes.zip(posfixes).map{
            case (prefix, posfix) => {
                if (prefix.equalsIgnoreCase(posfix)) prefix.length
                else -1
            }
        }
        val a = 1

    }

    def processQuery(query: String): Array[String] = {
        val tags = query.split("/")
        tags.slice(2, tags.length)
    }

    def main(args: Array[String]): Unit = {
        val queryStr = "//a/b/a/b/a/b/c/a"
        val queries = processQuery(queryStr)
        KMP(queries)

        val filename = "data/input.txt"
        for (line <- Source.fromFile(filename).getLines) {
            val arr = line.split(" ")
            val bit = Integer.parseInt(arr(0))
            val tag = arr(1)

            if (bit == 0) {
                startElement(tag)
            } else {
                endElement(tag)
            }
            index += 1
        }
    }

}
