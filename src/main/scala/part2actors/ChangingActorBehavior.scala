package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import part2actors.ActorCapabilities.{Counter, system}
import part2actors.ChangingActorBehavior.Mom.MomStart

object ChangingActorBehavior extends App{

  object FussyKid{
    case object KidAccept
    case object KidReject
    val HAPPY = "happy"
    val SAD = "sad"
  }
  class FussyKid extends Actor {
    import FussyKid._
    import Mom._
    var state = HAPPY
    override def receive: Receive = {
      case Food(VEGETABLE) => state = SAD
      case Food(CHOCOLATE) => state = HAPPY
      case Ask(message) =>
        if (state == HAPPY) sender() ! KidAccept
        else sender() ! KidReject
    }
  }

  class StatelessFussyKid extends Actor {
    import FussyKid._
    import Mom._
    override def receive: Receive = happyReceive
    def happyReceive: Receive = {
      case Food(VEGETABLE) => context.become(sadReceive, false) // change receive message handler
      case Food(CHOCOLATE) =>
      case Ask(_) => sender() ! KidAccept
    }
    def sadReceive: Receive = {
      case Food(VEGETABLE) => context.become(sadReceive, false)
      case Food(CHOCOLATE) => context.unbecome() // stack or discard
      case Ask(_) => sender() ! KidReject
    }
  }

  object Mom {
    case class MomStart(kidRef: ActorRef)
    case class Food(food: String)
    case class Ask(message: String) // do you want to play?
    val VEGETABLE = "veggies"
    val CHOCOLATE = "chocolate"
  }
  class Mom extends Actor {
    import Mom._
    import FussyKid._
    override def receive: Receive = {
      case MomStart(kidRef) =>
        //test interaction
        kidRef ! Food(VEGETABLE)
        kidRef ! Food(VEGETABLE)
        kidRef ! Food(CHOCOLATE)
        kidRef ! Food(CHOCOLATE)
        kidRef ! Ask("Do you want to play?")
      case KidAccept => println("Yay, my kid is happy!")
      case KidReject => println("My kid is sad, but at least he's healthy!")
    }
  }

  val system = ActorSystem("changingActorBehavior")
  val fussyKid = system.actorOf(Props[FussyKid])
  val mom = system.actorOf(Props[Mom])
  val statelessFussyKid = system.actorOf(Props[StatelessFussyKid])

  mom ! MomStart(statelessFussyKid)

  /*
  1 - counter actor with stack - create message handlers that might take parameters
   */

  object Counter {
    case object Increment
    case object Decrement
    case object Print
  }

  class Counter extends Actor {
    import Counter._
    override def receive: Receive = countReceive(0)

    def countReceive(currentCount: Int): Receive = {
      case Increment =>
        println(s"[$currentCount] incrementing")
        context.become(countReceive(currentCount + 1), false)
      case Decrement =>
        println(s"[$currentCount] decrementing")
        context.become(countReceive(currentCount - 1), false)
      case Print => println(s"[counter] current count is $currentCount")
    }
  }

  import Counter._
  val counter = system.actorOf(Props[Counter], "myCounter")
  (1 to 5).foreach(_ => counter ! Increment)
  (1 to 3).foreach(_ => counter ! Decrement)
  counter ! Print

  /*
  2 - simplified voting system
   */
  case class Vote ( candidate: String)
  case object VoteStatusRequest
  case class VoteStatusReply(candidate: Option[String])
  class Citizen extends Actor {
    override def receive: Receive = {
      case Vote(c) => context.become(voted(c))
      case VoteStatusRequest => sender() ! VoteStatusReply(None)
    }
    def voted(castedVote: String): Receive = {
      case VoteStatusRequest => sender() ! VoteStatusReply(Some(castedVote))
    }
  }
  // data structure to hold a Set of citizens
  case class AggregateVotes(citizens: Set[ActorRef])

  // receives a set of citizens,
  // count each nominee's number of votes
  // print the nominee's number of votes
  class VoteAggregator extends Actor {
    override def receive: Receive = {
      case AggregateVotes(citizens) =>
        citizens.foreach( _ ! VoteStatusRequest)
        context.become(agg(citizens, Map()))
    }

    def agg(remaining: Set[ActorRef], voteCount: Map[String, Int]) : Receive = {
      case VoteStatusReply(None) =>
        sender() ! VoteStatusRequest
      case VoteStatusReply(Some(candidate)) =>
        val newRemaining = remaining - sender()
        val currentVoteCt = voteCount.getOrElse(candidate, 0)
        val newVotes = voteCount + (candidate -> (currentVoteCt + 1))
        if (newRemaining.isEmpty) {
          println(s"[aggregator] poll stats: $newVotes")
        }
        else {
          context.become(agg(newRemaining, newVotes))
        }
    }
  }

  val alice = system.actorOf(Props[Citizen])
  val bob = system.actorOf(Props[Citizen])
  val charlie = system.actorOf(Props[Citizen])
  val dan = system.actorOf(Props[Citizen])

  alice ! Vote("Martin")
  bob ! Vote("Jonas")
  charlie ! Vote("Jonas")
  dan ! Vote("Roland")


  val voteAggregator = system.actorOf(Props[VoteAggregator])
  voteAggregator ! AggregateVotes(Set(alice, bob, charlie, dan))

  // see candidates with # of votes received


}
