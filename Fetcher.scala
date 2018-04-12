import scala.collection.mutable.Queue
import akka.actor.Actor
import akka.actor.Props
import akka.actor.ActorRef
import java.io._
import scala.io._
import scala.collection.mutable.Queue

/// Messages to pass
/// These do not have to be case classes,
///   but they often are

//For Reference from WordCount.scala
/*case class FileToCount(fileName: String)
case class WordCount(fileName: String, count: Int)
case class StartCounting(docRoot: String, numActors: Int)*/

//NEW:
case class StartCrawling(numActors: Int, pageWeightScheme: Int)
case class PageToFetch(requestURL: String)
//case class SearchQuery(qry: Query)


// WordCountWorker receives FileToCount messages,
// opens the file, counts the words,
// then returns a WordCount message
/*class WordCountWorker extends Actor {
 def countWords(fileName: String) = {
   val dataFile = new File(fileName)
   Source.fromFile(dataFile).getLines.foldLeft(0){ _+_ .split(" ").size }
 }*/

 class Fetcher extends Actor {
    var url: String = ""
    //TODO: Implement accordingly
    def fetchPages(requestURL: String) = {
      //TODO: get the Page based off requestURL
      println(requestURL)
      Page.fetchPage(requestURL)
    }
 
 // The abstract method of Actor that we have to implement
 // Use a pattern matching block
 def receive = {
   //NEW
   case PageToFetch(requestURL: String) => {
    //TODO: Do It
    url = requestURL
    //println("Fetcher is sending the Pages to the Manager")
    sender ! fetchPages(requestURL)
   }


  //For Reference from WordCount.scala
   // TODO: complete this with the instructor
     /*case FileToCount(fileName: String) => {
       val count = countWords(fileName)
       sender ! WordCount(fileName, count)
   }*/
 }
 
 // There are several methods of Actor we can override
 //   for startup/shutdown behavior
 override def postStop(): Unit = {
   //println(s"Worker actor is stopped: ${self}")
   // NOTE: self is another implicitly available variable for Actors
 }
 
}

//IndexManager
class IndexManager extends Actor {
  var urlsToFetch: Queue[String] = Queue[String]()
  var returnedPages: Queue[Page] = Queue[Page]()
  var index = new IndexedPages()
  


  def receive = {
    case StartCrawling(numActors: Int, pageWeightScheme: Int) => {
    	addTop50Pages()
      
      //Add to IndexPages the 50, then you add stuff to the queue as you remove from it

      val workers = createWorkers(numActors)
      val prompter = context.actorOf(Props[Prompter], name=s"worker-${numActors}")

      //call prompter, how?
      //workers(numActors) ! StartPrompting

      
      //prompter ! StartPrompting
      //println("Manager is trying to wake up prompter")

      prompter ! StartPrompting()

        //HAVE TO DEQUEUE AND QUEUE CONSTANTLY

      //call rest of workers
      //urlsToFetch is DYNAMIC -> QUESTION FOR DOCTOR WOLFE?!!!

      urlsToFetch.zipWithIndex.foreach( pr => {
      		//println("Manger setting up the orders for workers")
           workers(pr._2 % workers.size) ! PageToFetch(urlsToFetch.dequeue())
       })
    }
    /*case SearchQuery(qry: Query) => {

    }*/
    case q: Query => {
      //use terms in query to search
      val terms = q.getItems()
      if (terms.size < 1){
      	context.system.terminate()
      }
      else {
      	var sResults = index.search(q)
      	//println("Trying to send results to Prompter")
      	sender ! sResults
      }

      //if (terms.size)

    }

    //The Fetcher returned a page
    case op: Option[Page] => {
      if (op != None){
      	for (l <- op.get.links.take(5)){
      		urlsToFetch += l
      	}
      	println("added - url " + op.get.url)
      	index.add(op.get)
      	//returnedPages += op.get
      }
      	//returnedPages += op.getOrElse()
    }
  }

  override def postStop(): Unit = {
   println(s"Master actor is stopped: ${self}")
 }
 
 private def createWorkers(numActors: Int) = {
   // TODO: complete this with the instructor
     for (i <- 0 until numActors) yield
       context.actorOf(Props[Fetcher], name=s"worker-${i}")

 }

 def addTop50Pages() = {
  
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
	top50UrlsUsa.flatMap{ (u: String) => urlsToFetch += u }
}
}

// WordCountMaster
//   Finds files to count
//   Sends FileToCount messages to workers
//   Receives WordCount messages back, storing the results
// The class is also responsible for creating the worker actors
/*class WordCountMaster extends Actor {
 var fileNames: Seq[String] = Nil
 var wordCounts: scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map[String, Int]()
 
 def receive = {
   // TODO: complete this with the instructor
   case StartCounting(docRoot, numActors) => {
       val workers = createWorkers(numActors)
       fileNames = scanFiles(docRoot)
       fileNames.zipWithIndex.foreach( pr => {
           workers(pr._2 % workers.size) ! FileToCount(pr._1)
       })
   }
   
   case WordCount(fileName, count) => {
       wordCounts(fileName) = count
       
       if (wordCounts.size == fileNames.size){
           wordCounts = scala.collection.mutable.LinkedHashMap(
               wordCounts.toSeq.sortWith(_._2 >_ ._2):_*
               
               // :_* explodes the list into separate arguments
           )
       }
       println("final result " + wordCounts)
       context.system.terminate
   }
 }
 
 override def postStop(): Unit = {
   println(s"Master actor is stopped: ${self}")
 }
 
 private def createWorkers(numActors: Int) = {
   // TODO: complete this with the instructor
     for (i <- 0 until numActors) yield
       context.actorOf(Props[WordCountWorker], name=s"worker-${i}")
 }
 
 private def scanFiles(docRoot: String) =
   new File(docRoot).listFiles.filter(_.isFile).map{docRoot + "/" +_ }
}*/