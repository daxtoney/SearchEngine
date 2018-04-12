import akka.actor.ActorSystem
import akka.actor.Props


object Main {

  def main(args: Array[String]): Unit = {
    // TODO: complete this method
    val system = ActorSystem("search-engine-system")
    val m = system.actorOf(Props[IndexManager], name = "Master")
    m ! StartCrawling(15, 1)
    println("done with main")
    //TODO: Implement
    //val m = system.actorOf()     
  }
  
}