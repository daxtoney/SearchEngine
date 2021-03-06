import scala.util.matching.Regex

class IndexedPages_lu15() extends IndexedPages {

    //5 points to having two dashes, decrese by one until you have one for every extra dashes

    override def getWeights: Seq[Double] = {
        val pattern = new Regex("[/]")
        //val score: Double = 5.0 - (pattern findAllIn p.url).length
        val ret = for(p <- iPages) yield (getMax(1.0, 5.0 - (pattern findAllIn p.url).length))
            
        ret
    }
    
    def getMax(x: Double, y: Double): Double = {
        if (x > y) x else y
    }

}

class IndexedPages_d4ny() extends IndexedPages {

    override def getWeights: Seq[Double] = {

        val pointMutationsAllowed: Int = 4

        var weights: Seq[Double] = Seq[Double]()
        //for (p1 <- iPages) yield (iPages.foldLeft(0.0){if (_.links)})

        for (p <- iPages){
            var tempWeight: Double = 1.0
            for (p2 <- iPages){
                if (p2.links.contains(p.url)){
                    tempWeight = tempWeight + 1.0
                }
            }
            weights = weights :+ tempWeight
        }
        
        weights
    }


}
