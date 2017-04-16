package amora.backend

import scala.util.Failure
import scala.util.Success

import org.apache.log4j.ConsoleAppender
import org.apache.log4j.Level
import org.apache.log4j.LogManager
import org.apache.log4j.PatternLayout

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory

import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.UnhandledMessage
import akka.event.LogSource
import akka.event.Logging
import akka.event.LoggingAdapter
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import amora.backend.actors.UnhandledMessagesActor

/**
 * We need to depend on a log4j root logger because Jena uses log4j internally.
 * TODO Figure out how to unify Akka logging and Jena logging
 */
object Main extends AkkaLogging with Log4jRootLogging {
  private var _config: Config = _
  private var _system: ActorSystem = _

  def main(args: Array[String]) = {
    val path =
      if (args.length == 1)
        args.head
      else
        s"${System.getProperty("user.home")}/amora.conf"
    val f = new java.io.File(path)

    _config =
      if (f.exists())
        ConfigFactory.parseFile(f).resolve().withFallback(ConfigFactory.load())
      else
        ConfigFactory.load()

    _system = ActorSystem("amora", _config)
    implicit val system = _system

    if (f.exists())
      log.info(s"Succesfully read config file `$f`.")

    system.eventStream.subscribe(
        system.actorOf(Props[UnhandledMessagesActor], "unhandled-messages"),
        classOf[UnhandledMessage])

    implicit val materializer = ActorMaterializer()
    val service = new WebService
    val binding = Http().bindAndHandle(service.route, interface, port)

    import system.dispatcher
    binding.onComplete {
      case Success(binding) ⇒
        val addr = binding.localAddress
        log.info(s"Server is listening on ${addr.getHostName}:${addr.getPort}")
      case Failure(e) ⇒
        log.error(e, "Failed to start server")
        system.terminate()
    }
  }

  def system = _system
  def config = _config
  def interface = config.getString("app.interface")
  def port = config.getInt("app.port")
}

trait Log4jLoggingConfig {
  // Conversion characters: https://logging.apache.org/log4j/1.2/apidocs/org/apache/log4j/PatternLayout.html
  def consoleAppender = new ConsoleAppender(new PatternLayout("%d{ISO8601} %5p [%t] %c - %m%n"), ConsoleAppender.SYSTEM_OUT)
}

trait Log4jRootLogging extends Log4jLoggingConfig {
  val rootLogger = LogManager.getRootLogger
  rootLogger.setLevel(Level.INFO)
  rootLogger.addAppender(consoleAppender)
}

trait Log4jLogging extends Log4jLoggingConfig {
  lazy val log = {
    val l = LogManager.getLogger(getClass)
    l.setLevel(Level.INFO)
    l.setAdditivity(false)
    l.addAppender(consoleAppender)
    l
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
