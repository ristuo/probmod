package ennusta

import predict.Predict
import ennusta.io.CsvIo

object Main {
    def main(args: Array[String]) {
        val dataPath = "/home/risto/Documents/opiskelu/kurssit/probmodharkka/data/data3.txt"
        val data = CsvIo.readCsv(dataPath)
        
        var first = false
        var givenSoFar = new Array[Int](0)

        var prediction = new Array[Double](100)
        prediction(0) = 0.999
        for (i <- 1 until prediction.size)
            prediction(i) = 0.00001
        println(prediction.mkString(","))

        for (ln <- scala.io.Source.stdin.getLines) {
            if (first) {
                prediction(0) = 0.999
                for (i <- 1 until prediction.size)
                    prediction(i) = 0.00001
                println(prediction.mkString(","))
                givenSoFar = new Array[Int](0)
                first = false
            } else {
                givenSoFar = givenSoFar ++ Array(ln.toInt) 
                prediction = Predict.predict(givenSoFar=givenSoFar, data=data)
                println(prediction.mkString(","))
                if (givenSoFar.size == 302) {
                      first = true 
                }
            }
        }
    }
}
