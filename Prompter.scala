import akka.actor.Actor
import akka.actor.Props
import akka.actor.ActorRef

//DO WE NEED TO MODIFY THIS?
case class StartPrompting(){
  println("Started Prompting")
}

class Prompter extends Actor {
  private val numResultsToDisplay = 15
    
  def startQuery(mgr: ActorRef) = {
      // Prompt for the next query
      val q = scala.io.StdIn.readLine("Enter a query: ")
      
      // Make the query
      val qObj = new DictionaryQuery(if(q.trim.length == 0) Nil else q.trim.split("(_|\\W)+").map(_.toLowerCase))
      
      // Send to the manager
      mgr ! qObj
  }
      
  
  // To add logging of messages received,
  //   1. edit the application.conf file
  //   2. use the following line instead of def receive = {
  //def receive = akka.event.LoggingReceive {
  def receive = {
    //println("Prompter is awake")

    //WOLFE QUESTION, HOW DO WE CALL START AND WHERE (WHEN WE CREATE THE PROMPTER?)
    case start: StartPrompting => startQuery(sender)
    case res: SearchResults => {
      println("Searched for [" + res.query + "] in " + res.numIndexedPages + " web pages...")
      res.top(numResultsToDisplay).foreach{ case (url, score) => printf("%10.4f   %s\n", score, url) }
      println("")
      
      startQuery(sender)
    }
    case _ => startQuery(sender)
  }
}