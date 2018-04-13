import scala.math._

class IndexedPages() extends Seq[Page] with Weighted[Page] {
    var iPages: Seq[Page] = Seq[Page]()

    def add(p: Page) = {
        /*Is this right?*/
        //println(p.url)
        if(!iPages.contains(p)){
            iPages = iPages :+ p
        }
    }

    def search(qry: Query): SearchResults = {
        // How do I use foldLeft here, I know I need to
        // Same with the weights, how do I use those here
        val scores = for (p <- iPages) yield getScore(qry, p).foldLeft(0.0)(_ + _)

        val retPWts = scores.zip(getWeights)
        val actualScores = retPWts.map(x => (x._1 * x._2))
        //yield for( q <- qry ) yield (tf(q,p,(for (q <- qry) yield p.text.split("\\s+").count(_ == q)).foldLeft(0)((x: Int, y: Int) => { _ + _ } ) )*idf(iPages.length, q))

        //val sch = new SearchResults(qry, 0, iPages.map(_.url), scores)
        val sch = new SearchResults(qry, 0, iPages.map(_.url), actualScores)

        sch

        /*  
            TF(t) = (Number of times term t appears in a document) / (Total number of words in the document)
            IDF(t) = log_e(Total number of documents / Number of documents with term t in it).
            Value = TF * IDF
        */
    }

    def getScore(qry: Query, p: Page): Seq[Double] = {
        p.splitText = p.text.split("\\s+")
        val ret = for( q <- qry.getItems ) yield (tf(q, p, p.splitText.size) * idf(iPages.length, q))
        val retAWts = ret.zip(qry.getWeights)
        val ter = retAWts.map(x => (x._1 * x._2))
        //println(p.url)

        //ret
        ter
    }

    def totalTerms(qry: Query, p: Page): Int = {
        val ret = (for (q <- qry.getItems) yield p.splitText.count(_ == q)).foldLeft(0)((x: Int, y: Int) => { x + y } )
        ret
    }

    def tf(term: String, p: Page, totalTerms: Int): Double = {
        //println("Page URL: " + p.url + " -- Word Count: " + p.text.split("\\s+").count(_ == term) + " -- Total Terms" + totalTerms)
        ( ( p.splitText.count(_ == term) * 1.0 ) / ( 1 + totalTerms ) )
    }

    def idf(docCount: Int, term: String): Double = {
        log(docCount * 1.0 / ( 1 + iPages.count( _.splitText.contains(term) ) ) )
    }

    def index_=(ps: Seq[Page]): Unit = {

    }
    // Members declared in scala.collection.IterableLike
    def iterator(): Iterator[Page] = {
        iPages.iterator
    }
    // Members declared in scala.collection.SeqLike
    def apply(idx: Int): Page = {
        iPages(idx)
    }
    def length: Int = {
        iPages.length
    }
    // Members declared in Weighted
    def getItems(): Seq[Page] = {
        iPages
    }
    def getWeights: Seq[Double] = {
        iPages.map((x: Page) => 1.0)
    }
    def addPageChildren(p: Page): Seq[Page] = {
        //val pageDoc = Page.getDocument(p.url)
        val links = Page.getLinks(p.doc)
        var pageSeq: Seq[Page] = Seq[Page]()

        for (l <- links.take(4) ) {
            val newPage = Page.fetchPage(l)
            add(Page.fetchPage(l).getOrElse(p))
            pageSeq :+ newPage
        }

        pageSeq
    }

}

/*class Query(strs: Seq[String]) extends Weighted[String] {
    def getItems(): Seq[String] = {
        strs
    }
    def getWeights(): Seq[Double] = {
        strs.map(_.length.toDouble)
    }
}*/

/*case q: Query => {
    val terms = q.terms
    if(terms.soze == 0){
        context.system.terminate
    }else{
        val results = IndexedPages.search(q)
        sender ! results
    }
}*/

object MainNoActors {
  def main(args: Array[String]) = {
    //val index = new IndexedPages()
    //val index = new IndexedPages_lu15()
    val index = new IndexedPages_lu15()
    addTop50Pages(index)
    
    val queries = Vector( Vector("news"),
                          Vector("apple"),
                          Vector("sports", "ncaa"),
                          //Vector("watch", "movies"),
                          Vector("friend", "love"),
                          //Vector("watch", "movies") ).map{ new Query(_) }
                          Vector("war", "pain") ).map{ new DictionaryQuery(_) }
                          
    for(q <- queries) {
      val results = index.search(q)
      println(q)
      println(q.getWeights())
      //println(q.loadBad())
      //println(q.loadGood())
      results.top(8).foreach{ case (url, score) => printf("%10.4f   %s\n", score, url) }
      println("")
    }
  }
  
  def addTop50Pages(index: IndexedPages) = {
  
    // from http://www.alexa.com/topsites/countries/US
    val top50UrlsUsa = Vector(
    "google.com",
    "youtube.com", 
    "facebook.com",
    "amazon.com",
    "yahoo.com",
    "wikipedia.org",
    "reddit.com",
    "twitter.com",
    "ebay.com",
    "linkedin.com",
    "netflix.com",
    "diply.com",
    "instagram.com",
    "live.com",
    "craigslist.org",
    "bing.com",
    "imgur.com",
    "ntd.tv",
    "cnn.com",
    "pinterest.com",
    "tumblr.com",
    "office.com",
    "microsoftonline.com",
    "t.co",
    "chase.com",
    "nytimes.com",
    "blogspot.com",
    "imdb.com",
    "paypal.com",
    // omitted: "livejasmin.com",
    // omitted: "pornhub.com",
    "wordpress.com",
    "espn.com",
    "apple.com",
    "breitbart.com",
    "msn.com",
    "walmart.com",
    "wikia.com",
    "bankofamerica.com",
    "salesforce.com",
    "wellsfargo.com",
    "washingtonpost.com",
    "weather.com",
    "intuit.com",
    "huffingtonpost.com",
    "zillow.com",
    "microsoft.com",
    "instructure.com",
    "foxnews.com",
    "twitch.tv"
    ).map( (base: String) => "http://" + base )
    
    val pagesToAdd = top50UrlsUsa.flatMap{ (u: String) => Page.fetchPage(u) }
        
    // uncomment to see the content of the pages  
    //for(p <- pagesToAdd) {println(p.url); println(p.text); println("\n\n")}
    
    for(p <- pagesToAdd) {
        index.add(p)
        index.addPageChildren(p) // Crawl 1 level 
    }
  }
}