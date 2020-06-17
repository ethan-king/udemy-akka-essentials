package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ChildActors extends App {

  object Parent {
    case class CreateChild(name: String)
    case class TellChild(message: String)
  }

  class Parent extends Actor {
    import Parent._
    override def receive: Receive = {
      case CreateChild(name) =>
        println(s"${self.path} creating child")
        val childRef = context.actorOf(Props[Child], name) // create child then
        context.become(withChild(childRef)) // change Receive handler to withChild

    }
    def withChild(childRef: ActorRef): Receive = {
      case TellChild(message) =>
        if (childRef != null) childRef forward message
    }
  }

  class Child extends Actor {
    override def receive: Receive = {
      case message => println(s"${self.path} I got: $message")
    }
  }

  import Parent._

  val system = ActorSystem("ParentChildDemo")
  val parent = system.actorOf(Props[Parent])
  parent ! CreateChild("child")
  parent ! TellChild("do your chores")

  // actor hierarhchies
  // parent -> child -> grandchild
  //        -> child2

  //

  /*
  Guardian actors - system - top-level actors
  - /system : system guardian
  - /user : anything created by user
  - / : root guardian
   */

  /**
   *  Actor selection
   */
  val childSelection = system.actorSelection("/user/parent/child")
  childSelection ! "I found you!"
}
