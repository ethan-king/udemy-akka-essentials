package part2actors

import akka.actor.{Actor, ActorSystem, Props}

object ActorsIntro extends App {

  //part1 - actor systems
  val actorSystem = ActorSystem("firstActorSystem")
  println(actorSystem.name)

  // part 2 - create actor
  // actors are unique id's
  // message are async
  // each actor may respond differently
  // cannot invade their mind

  class WordCountActor extends Actor {
    var totalWords = 0
    def receive: PartialFunction[Any, Unit] = {
      case message: String =>
        println(s"[word counter] I have received: $message")
        totalWords += message.split(" ").length
      case msg => println(s"[word counter] I cannot understand ${msg.toString}")
    }
  }

  // part3 - instantiate actor
  val wordCounter = actorSystem.actorOf(Props[WordCountActor], "wordCounter")
  val anotherWordCounter = actorSystem.actorOf(Props[WordCountActor], "anotherWordCounter")

  // communicate w/ actor
  wordCounter ! "Hooray Akka fo shizzle!"
  anotherWordCounter ! "A different message..."

  class Person(name: String) extends Actor {
    override def receive: Receive = {
      case "hi" => println(s"Hi, my name is $name")
      case _ =>
    }
  }

  val person = actorSystem.actorOf(Person.props("Bob"))
  person ! "hi"

  // best practice is to call new props with companion object that has a factory method, props
  object Person {
    def props(name: String) = Props(new Person(name))
  }
  // call Person.props("Timmy")
}

