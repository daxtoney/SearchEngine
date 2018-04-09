class IndexedPages() extends Seq[Page] with Weighted[Page] {
    var iPages: Seq[Page] = Seq[Page]()

    def add(p: Page) = {
        /*Is this right?*/
        if(!iPages.contains(p)){
            iPages = iPages :+ p
        }
    }

    def score(iPages: Seq[Page]): Seq[Double] = {
        iPages.map(_.url.length.toDouble)
    }

    def search(qry: Query): SearchResults = {
        // How do I use this
        val sch = new SearchResults(qry, 0, iPages.map(_.url), score(iPages))

        // How do I use foldLeft here, I know I need to
        // Same with the weights, how do I use those here
        for (p <- iPages) {
            for (q <- qry) {
                p.count(getOrElse(q, 0))
            }
        }

        /*  
            TF(t) = (Number of times term t appears in a document) / (Total number of terms in the document)
            IDF(t) = log_e(Total number of documents / Number of documents with term t in it).
            Value = TF * IDF
        */

        sch
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
        1.0
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

object MainNoActors {
  def main(args: Array[String]) = {
    val index = new IndexedPages()
    addTop50Pages(index)
    
    val queries = Vector( Vector("news"),
                          Vector("apple"),
                          Vector("sports", "ncaa"),
                          Vector("watch", "movies") ).map{ new Query(_) }
                          
    for(q <- queries) {
      val results = index.search(q)
      println(q)
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
    "twitch.tv").map( (base: String) => "http://" + base )
    
    val pagesToAdd = top50UrlsUsa.flatMap{ (u: String) => Page.fetchPage(u) }
        
    // uncomment to see the content of the pages  
    //for(p <- pagesToAdd) {println(p.url); println(p.text); println("\n\n")}
    
    for(p <- pagesToAdd) index.add(p)
  }
}