package ennusta.io
import scala.io.Source

object CsvIo {

    def readCsv(dataPath: String): Array[Array[Int]] = {
        val rows = countLines( dataPath )  
        val res = new Array[Array[Int]](rows)
        var i = 0
        for (line <- Source.fromFile(dataPath).getLines) {
            res(i) = line.split(",").map(_.toInt)
            i = i+1
        }
        res
    }

    def countLines(dataPath: String): Int = {
        val src = Source.fromFile( dataPath )
        try {
            src.getLines.size
        } finally {
            src.close
        }
    }

    def countCols(dataPath: String): Int = {
        val src = Source.fromFile(dataPath)
        try {
            src.getLines.find(_ => true).getOrElse("").split(",").size
        } finally {
            src.close()
        }
    }
}
