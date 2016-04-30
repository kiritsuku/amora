package backend

import scala.util.Failure
import scala.util.Success

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
  override implicit val system = ActorSystem("backend")
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
