package ennusta.predict

object Predict {

    def manhattan(
        dataRow: Array[Int],
        givenSoFar: Array[Int]
    ): Double = {
        require(givenSoFar.size <= dataRow.size,"plz too long data row")
        var res: Double = 0
        for (i <- 0 until givenSoFar.size) {
            res += Math.abs(dataRow(i) - givenSoFar(i))
        }
        res
    }

    def predict(
        givenSoFar: Array[Int], 
        data: Array[Array[Int]],
        p: Double = 1
    ): Array[Double] = {
        val res = new Array[Double](100)
        val currentCol = givenSoFar.size
        val dists: Array[(Array[Int], Double)] = data.map(dataRow => {
            (
                dataRow, 
                manhattan(dataRow=dataRow,givenSoFar=givenSoFar)
            )
        })
        val cutoff = 75
        val colVals = { 
            val filsu1 = dists
                .filter(x => x._2 <= cutoff)
                .map(_._1(currentCol))
            if (filsu1.size > 0) {
                filsu1
            } else {
                val sorted = dists.sortBy(_._2)
                dists
                    .filter(x => x._2 <= sorted(3)._2)
                    .map(_._1(currentCol))
            }
        }
        colVals.foreach(colval => {
            res(colval) += 1
        })
        res.map(x => ( x + 0.0001 )/res.sum)
    }
}
