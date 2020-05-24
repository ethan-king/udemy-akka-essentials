package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import part2actors.ActorCapabilities.Person.LiveTheLife

object ActorCapabilities extends App {
  class SimpleActor extends Actor {
    override def receive: Receive = {
      case "hi" => context.sender() ! "Hello, there!" // context.sender is a reply
      case message: String => println(s"[simple actor][$self] I have received $message")
      case number: Int => println(s"[simple actor] I have received a NUMBER: $number")
      case SpecialMessage(contents) => println(s"[simple actor] I have received something SPECIAL: $contents")
      case SayHiTo(ref) => ref ! "hi"
      case SendMessageToYourself(content) => self ! content
      case WirelessPhoneMessage(content, ref) => ref forward (content + "s"+ s"[sender] $sender")
    }
  }

  val system = ActorSystem("actorCapabilitiesDemo")
  val simpleActor = system.actorOf(Props[SimpleActor], "simpleActor")

//  simpleActor ! "hello, actor"

  // 1 - messages can be any type
  // must be IMMUTABLE and serializable
  // use case class and case object
//  simpleActor ! 42

  case class SpecialMessage(contents: String)
//  simpleActor ! SpecialMessage("Special content")

  // 2 -  actors have info about their context and about themselves
  // context.self === `this` in OOP

  case class SendMessageToYourself(content: String)
//  simpleActor ! SendMessageToYourself("I'm an actor")

  // 3 - actors can reply to messages
  val alice = system.actorOf(Props[SimpleActor], "alice")
  val bob = system.actorOf(Props[SimpleActor], "bob")

  case class SayHiTo(ref: ActorRef)
  alice ! SayHiTo(bob)

  // 4 - dead letters
//  alice ! "hi" //reply to "me", but an actor didn't send this message

  // 5 - forward message - send message with original sender

  case class WirelessPhoneMessage(content: String, ref: ActorRef)
  alice ! WirelessPhoneMessage("What", bob)

  // counter class
  object Counter {
    case object Increment
    case object Decrement
    case object Print

  }
  class Counter extends Actor {
    import Counter._
    var count = 0

    override def receive: Receive = {
      case Increment => count += 1
      case Decrement => count -= 1
      case Print => println(s"[counter] My current count is $count")
    }
  }

  import Counter._
  val counter = system.actorOf(Props[Counter], "myCounter")
  (1 to 5).foreach(_ => counter ! Increment)
  (1 to 3).foreach(_ => counter ! Decrement)
  counter ! Print

  // bank account
  object BankAccount {
    case class Deposit(amount: Int)
    case class Withdraw(amount: Int)
    case object Statement

    case class TransactionSuccess(message: String)
    case class TransactionFailure(reason: String)
  }

  class BankAccount extends Actor {
    import BankAccount._
    var funds = 0

    override def receive: Receive = {
      case Deposit(amount) =>
        if (amount <0) sender() ! TransactionFailure("invali deposit amount")
        else {
          funds += amount
          sender () ! TransactionSuccess( s"successfully deposited $amount")
        }
      case Withdraw(amount) =>
        if (amount <0) sender() ! TransactionFailure("invalid withdraw amount")
        else if (amount > funds) sender() ! TransactionFailure("insufficient funds")
        else {
          funds -= amount
          sender() ! TransactionSuccess(s"successfully withdraw $amount")
        }

      case Statement => sender() ! s"Your balance is $funds"
    }
  }
  object Person {
    case class LiveTheLife(account: ActorRef)
  }
  class Person extends Actor {
    import Person._
    import BankAccount._

    override def receive: Receive = {
      case LiveTheLife(account) =>
        account ! Deposit(10000)
        account ! Withdraw(50000)
        account ! Withdraw(500)
        account ! Statement
      case message => println(message.toString)
    }

  }

  val account = system.actorOf(Props[BankAccount] , "bankAccount")
  val person = system.actorOf(Props[Person], "billionaire")

  person ! LiveTheLife(account)
}
