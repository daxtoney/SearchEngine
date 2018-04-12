import scala.util.matching.Regex

class IndexedPages_lu15() extends IndexedPages {

    //5 points to having two dashes, decrese by one until you have one for every extra dashes

    override def getWeights: Seq[Double] = {
        val pattern = new Regex("[/]")
        //val score: Double = 5.0 - (pattern findAllIn p.url).length
        val ret = for(p <- iPages) yield (getMax(1.0, 5.0 - (pattern findAllIn p.url).length))

        /*for (r <- ret){
            println(r)
        }*/

        ret
        //iPages.map((x: Page) => 1.0)
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
        for (p <- iPages; p2 <- iPages/*; l <- p2.links*/){
            var tempWeight: Double = 1.0

            if (p2.links.contains(p.url)){
                tempWeight = tempWeight + 1.0
            }
            /*if (p.url.zip(l.dropRight(l.length - p.url.length)).count{case (a,b) => a != b} <= pointMutationsAllowed){
                tempWeight = tempWeight + 1
            }*/


            weights = weights :+ tempWeight
        }
        for (w <- weights){
            if (w > 1.0){
                print(w)
            }
            
        }
        weights
    }


}
