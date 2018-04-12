import scala.collection.mutable.Queue
import akka.actor.Actor
import akka.actor.Props
import akka.actor.ActorRef
import java.io._
import scala.io._

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
      Page.fetchPage(requestURL)
    }
 
 // The abstract method of Actor that we have to implement
 // Use a pattern matching block
 def receive = {
   //NEW
   case PageToFetch(requestURL: String) => {
    //TODO: Do It
    url = requestURL
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
   println(s"Worker actor is stopped: ${self}")
   // NOTE: self is another implicitly available variable for Actors
 }
 
}

//IndexManager
class IndexManager extends Actor {
  var urlsToFetch: Queue[String] = Queue[String]()
  var returnedPages: Queue[Page] = Queue[Page]()


  def receive = {
    case StartCrawling(numActors: Int, pageWeightScheme: Int) => {
      
      val workers = createWorkers(numActors)

      //call prompter, how?
      //workers(numActors) ! StartPrompting



      //call rest of workers
      //urlsToFetch is DYNAMIC -> QUESTION FOR DOCTOR WOLFE?!!!
      urlsToFetch.zipWithIndex.foreach( pr => {
           //workers(pr._2 % workers.size) ! PageToFetch(pr._1)
       })
    }
    /*case SearchQuery(qry: Query) => {

    }*/
    case q: Query => {
      //use terms in query to search
    }

    //The Fetcher returned a page
    case op: Option[Page] => {
      var value: Page = op.get
      value match {
        //case None => empty()
        case _ => returnedPages += value
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

     //Create one prompter
     context.actorOf(Props[Prompter], name=s"worker-${numActors}")

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