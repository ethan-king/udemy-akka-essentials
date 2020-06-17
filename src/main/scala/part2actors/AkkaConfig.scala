package part2actors

import akka.actor.{Actor, ActorLogging, ActorSystem}
import com.typesafe.config.ConfigFactory

object AkkaConfig extends App {
  // inline config
  class SimpleLoggingActor extends Actor with ActorLogging {
    override def receive: Receive = {
      case message => log.info(message.toString)
    }
  }

  // akka config begins with akka namespace
  val configString =
    """
      | akka {
      |   loglevel = "DEBUG"
      | }
      |""".stripMargin

  val config = ConfigFactory.parseString(configString)
  val system = ActorSystem("ConfigurationDemo", ConfigFactory.load(config))
}
