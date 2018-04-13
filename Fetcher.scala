import scala.collection.mutable.Queue
import akka.actor.Actor
import akka.actor.Props
import akka.actor.ActorRef
import java.io._
import scala.io._
import scala.collection.mutable.Queue

//NEW:
case class StartCrawling(numActors: Int)
case class PageToFetch(requestURL: String)
//case class SearchQuery(qry: Query)


 class Fetcher extends Actor {
 
 // The abstract method of Actor that we have to implement
 // Use a pattern matching block
  def receive = {
   //NEW
   case PageToFetch(requestURL: String) => {
    sender ! Page.fetchPage(requestURL)
  }
 }
 


   // There are several methods of Actor we can override
   //   for startup/shutdown behavior
   override def postStop(): Unit = {
     // NOTE: self is another implicitly available variable for Actors
   }

 }

 //IndexManager
 class IndexManager extends Actor {
	  var urlsToFetch: Queue[String] = Queue[String]()
	  var returnedPages: Queue[Page] = Queue[Page]()
	  var index = new IndexedPages_lu15()

	  def receive = {
	    case StartCrawling(numActors: Int) => {
	    	addTop50Pages()

	      //val workers = createWorkers(numActors)
	      val prompter = context.actorOf(Props[Prompter], name=s"worker-${numActors}")

	      val workers = createWorkers(numActors)

	      
	      //prompter ! StartPrompting
	      println("Manager is trying to wake up prompter")

	      prompter ! StartPrompting()

	      	urlsToFetch.zipWithIndex.foreach( pr => {
	           workers(pr._2 % workers.size) ! PageToFetch(pr._1)
	       })
	      
	    }

	    case q: Query => {
	      //use terms in query to search
	      val terms = q.getItems()
	      if (terms.size < 1){
	      	println("Terminating the system")
	      	context.system.terminate()
	      }
	      else {
	      	var sResults = index.search(q)
	      	sender ! sResults
	      }
	    }

	    //The Fetcher returned a page
	    case op: Option[Page] => {
	    	if (op != None){
	    		index.add(op.get)
	    		for (l <- op.get.links.take(5)){
	      			urlsToFetch += l
	      		}
	    	}
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
