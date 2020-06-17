package part2actors

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.event.Logging

object ActorLoggingDemo extends App{

  // 1 explicit logging
  class SimpleActorWithExplicitLogger extends Actor {
    val logger = Logging(context.system, this) // apply method takes actor system
    override def receive: Receive = {
      /*
       logging - 4 levels
       1 - debug
       2 - info
       3 - warning
       4 - error
       */
      case message => logger.info(message.toString) // log level 2 - info
    }
  }
  val system = ActorSystem("LoggingDemo")
  val actor = system.actorOf(Props[SimpleActorWithExplicitLogger])
  actor ! "Logging a simple message"

  // #2 - ActorLogging
  class ActorWithLogging extends Actor with ActorLogging {
    override def receive: Receive = {
      case (a,b) => log.info("Two things: {} and {}", a,b)
      case message => log.info(message.toString)
    }
  }
  val simplerActor = system.actorOf(Props[ActorWithLogging])
  simplerActor ! "Logging a simple message by extending a trait"

  // interpolating params
  // send a tuple
  simplerActor ! (42, 65)

  // Logging is asynchronous
  // Logging doesn't depend on the logging implementation
}
