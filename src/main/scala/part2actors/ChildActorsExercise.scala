package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ChildActorsExercise extends App {
  // distributed word count

  object WordCounterMaster {
    case class Initialize(nChildren: Int)
    case class WordCountTask(id: Int, text: String)
    case class WordCountReply(id: Int, count: Int)
  }

  class WordCounterMaster extends Actor {
    import WordCounterMaster._
    override def receive: Receive = {
      case Initialize(nChildren) =>
        // create system of actors, context change
        val childrenRefs = for (i <- 1 to nChildren) yield context.actorOf(Props[WordCounterWorker], s"wcw_$i")
        context.become(withChildren(childrenRefs, 0, 0, Map()))
    }

    def withChildren(childrenRefs: Seq[ActorRef], currentChildIndex: Int, currentTaskId: Int, requestMap: Map[Int, ActorRef]): Receive = {
      case text: String =>
        // send messsage to child
        val originalSender = sender()
        val task = WordCountTask(currentTaskId, text)
        val childRef = childrenRefs(currentChildIndex)
        childRef ! task
        // change state
        val nextChildIndex = (currentChildIndex +1 ) % childrenRefs.length // round robin - restart at beginning of list
        val newTaskId = currentTaskId + 1
        val newRequestMap = requestMap + (currentTaskId -> originalSender)
        context.become(withChildren(childrenRefs, nextChildIndex, newTaskId, newRequestMap))
      case WordCountReply(id, count) => // keep track of which reply corresponds to which request
        println(s"[master] I have received a reply for task id $id with $count")
        val originalSender = requestMap(id)
        originalSender ! count
        context.become(withChildren(childrenRefs, currentChildIndex, currentTaskId, requestMap - id)) // process and remove task id

    }

  }

  class WordCounterWorker extends Actor {
    import WordCounterMaster._
    override def receive: Receive = {
      case WordCountTask(id, text) =>
        println(s"${self.path} I have received task $id with $text")
        sender() ! WordCountReply(id, text.split(" ").length)
    }
  }


  class TestActor extends Actor {
    import WordCounterMaster._

    override def receive: Receive = {
      case "go" =>
        val master = context.actorOf(Props[WordCounterMaster], "master")
        master ! Initialize(3)
        val text = List(" I love Akka", "Scala is fun yes", "yes", "me too")
        text.foreach(text => master ! text)
      case count: Int =>
        println(s"[test actor] I received a reply: $count")
    }
  }

  val system = ActorSystem("roundRobinWordCount")
  val testActor = system.actorOf(Props[TestActor], "testActor")
  testActor ! "go"
  /*
  create WordCounter Master
  send Init(10) to wordCounterMaster
  send "Akka is awesome" to wrodCounterMaster
    wcv will send a WCTask to one of its children
      child replies with WC Reply to master
    master replies to sender

  requester -> wcm -> wcw
      r <- wcv <-
  round robin logic

   */
}
