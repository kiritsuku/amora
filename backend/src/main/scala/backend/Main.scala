package backend

import scala.util.Failure
import scala.util.Success

import org.apache.log4j.ConsoleAppender
import org.apache.log4j.Level
import org.apache.log4j.LogManager
import org.apache.log4j.PatternLayout

import com.typesafe.config.ConfigFactory

import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.UnhandledMessage
import akka.event.LogSource
import akka.event.Logging
import akka.event.LoggingAdapter
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import backend.actors.UnhandledMessagesActor

object Main extends App with AkkaLogging {
  // Conversion characters: https://logging.apache.org/log4j/1.2/apidocs/org/apache/log4j/PatternLayout.html
  // TODO This logging stuff is only needed for Jena. We should merge Jena and Akka log output.
  val layout = new PatternLayout("%d{ISO8601} %5p [%t] %c - %m%n")
  val consoleAppender = new ConsoleAppender(layout, ConsoleAppender.SYSTEM_OUT)
  val rootLogger = LogManager.getRootLogger
  rootLogger.setLevel(Level.INFO)
  rootLogger.addAppender(consoleAppender)

  override implicit val system = ActorSystem("backend")
  implicit val materializer = ActorMaterializer()
  import system.dispatcher

  val config = {
    val f = new java.io.File(s"${System.getProperty("user.home")}/application.conf")
    if (f.exists()) {
      log.info(s"Reading config file `$f`")
      ConfigFactory.parseFile(f).withFallback(system.settings.config)
    }
    else
      system.settings.config
  }
  val interface = config.getString("app.interface")
  val port = config.getInt("app.port")

  def ServerAddress = s"$interface:$port"

  system.eventStream.subscribe(
      system.actorOf(Props[UnhandledMessagesActor], "unhandled-messages"),
      classOf[UnhandledMessage])

  val service = new WebService

  val binding = Http().bindAndHandle(service.route, interface, port)
  binding.onComplete {
    case Success(binding) ⇒
      val addr = binding.localAddress
      log.info(s"Server is listening on ${addr.getHostName}:${addr.getPort}")
    case Failure(e) ⇒
      log.error(e, "Failed to start server")
      system.terminate()
  }
}

trait AkkaLogging {
  private var _log: LoggingAdapter = _

  private implicit val logSource: LogSource[AnyRef] = new LogSource[AnyRef] {
    override def genString(o: AnyRef): String = o.getClass.getName
    override def getClazz(o: AnyRef): Class[_] = o.getClass
  }

  def system: ActorSystem

  def log: LoggingAdapter = {
    if (_log eq null)
      _log = Logging(system, this)
    _log
  }
}
