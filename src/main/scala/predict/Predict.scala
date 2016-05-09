package ennusta.predict

object Predict {
    def predict(
        givenSoFar: Array[Int], 
        data: Array[Array[Int]]
    ): Array[Double] = {
        val res = new Array[Double](100)
        val currentCol = givenSoFar.size
        val dists: Array[(Array[Int], Double)] = data.map(dataRow => {
            (
                dataRow, 
                dist(dataRow=dataRow,givenSoFar=givenSoFar)
            )
        }).sortBy(_._2)
        val cutoff = dists(150)._2
        val colVals = dists
            .filter(x => x._2 <= cutoff)
            .map(_._1(currentCol))
        colVals.foreach(colval => {
            res(colval) += 1
        })
        res.map(x => ( x + 0.0001 )/res.sum)
    }

    def predictWithWeights(
        givenSoFar: Array[Int], 
        data: Array[Array[Int]]
    ): Array[Double] = {
        val res = new Array[Double](100)
        val currentCol = givenSoFar.size
        val dists: Array[(Array[Int], Double)] = data.map(dataRow => {
            (
                dataRow, 
                dist(dataRow=dataRow,givenSoFar=givenSoFar)
            )
        }).sortBy(_._2)
        val cutoff = dists(150)._2
        val fdists = dists
            .filter(_._2 <= cutoff)
        val sumOfDists = fdists.map(_._2).sum
        val weights = fdists.map(x => 1/((x._2+100)/sumOfDists))
        val sumOfWeights = weights.sum
        val finalWeights = weights.map(_ / sumOfWeights)
        val colVals = fdists
            .map(_._1(currentCol)).zip(finalWeights)
        colVals.foreach(colval => {
            res(colval._1) += colval._2
        })
        if (res.filter(_.isNaN).size > 0) {
            res(0) = 0.9999
            for (i <- 1 until 100) {
                res(i) = 0.0001
            }
            return(res)
        }
        res.map(x => ( x + 0.00001 )/(res.sum))
    }

    def dist(
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
}
