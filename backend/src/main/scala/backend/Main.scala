package backend

import scala.util.Failure
import scala.util.Success

import org.apache.log4j.ConsoleAppender
import org.apache.log4j.Level
import org.apache.log4j.LogManager
import org.apache.log4j.PatternLayout

import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.UnhandledMessage
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import backend.actors.UnhandledMessagesActor

object Main extends App with LoggerConfig {
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  import system.dispatcher

  val config = system.settings.config
  val interface = config.getString("app.interface")
  val port = config.getInt("app.port")

  system.eventStream.subscribe(
      system.actorOf(Props[UnhandledMessagesActor], "unhandled-messages"),
      classOf[UnhandledMessage])

  val service = new WebService

  val binding = Http().bindAndHandle(service.route, interface, port)
  binding.onComplete {
    case Success(binding) ⇒
      val addr = binding.localAddress
      system.log.info(s"Server is listening on ${addr.getHostName}:${addr.getPort}")
    case Failure(e) ⇒
      system.log.error(e, "Failed to start server")
      system.terminate()
  }
}

trait LoggerConfig {
  val layout = new PatternLayout("%d %5p [%t] - %c - %m%n")
  val consoleAppender = new ConsoleAppender(layout, ConsoleAppender.SYSTEM_OUT)
  val rootLogger = LogManager.getRootLogger
  rootLogger.setLevel(Level.INFO)
  rootLogger.addAppender(consoleAppender)
}
