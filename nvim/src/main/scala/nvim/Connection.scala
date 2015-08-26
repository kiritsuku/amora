package nvim

import java.util.concurrent.atomic.AtomicInteger

import scala.collection.immutable
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.stream.ActorMaterializer
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.Flow
import akka.stream.scaladsl.Sink
import akka.stream.scaladsl.Source
import akka.stream.scaladsl.Tcp
import akka.util.ByteString
import msgpack4z._
import nvim.internal._

final class Connection(host: String, port: Int)(implicit system: ActorSystem) {

  private var notificationHandlers = List[Notification ⇒ Unit]()
  private val notifyHandlers: Notification ⇒ Unit = { n ⇒
    notificationHandlers foreach (_(n))
  }

  private val connectionActor = system.actorOf(Props(classOf[ConnectionActor], host, port, notifyHandlers))
  private val gen = new IdGenerator

  def addNotificationHandler(handler: Notification ⇒ Unit): Unit = {
    notificationHandlers +:= handler
  }

  def removeNotificationHandler(handler: Notification ⇒ Unit): Unit = {
    notificationHandlers = notificationHandlers.filterNot(_ == handler)
  }

  def sendNotification[A]
      (method: String, params: MsgpackUnion*)
      : Unit = {
    val ps = MsgpackUnion.array(params.toList)
    val req = Notification(2, method, ps)
    connectionActor ! req
  }

  def sendRequest[A]
      (command: String, params: MsgpackUnion*)
      (converter: PartialFunction[MsgpackUnion, A])
      (implicit ec: ExecutionContext)
      : Future[A] = {
    val id = gen.nextId()
    val ps = MsgpackUnion.array(params.toList)
    val req = Request(0, id, command, ps)

    val p = Promise[A]
    val f: Response ⇒ Unit = { resp ⇒
      if (!converter.isDefinedAt(resp.result))
        p.failure(new UnexpectedResponse(resp.result.toString))
      else
        Try(converter(resp.result)) match {
          case Success(value) => p.success(value)
          case Failure(f) => p.failure(f)
        }
    }
    connectionActor ! Msg(req, f)

    p.future
  }
}

private final case class Msg(req: Request, f: Response ⇒ Unit)

private final class ConnectionActor(host: String, port: Int, notificationHandler: Notification ⇒ Unit) extends Actor {
  val ConnectionLost = "ConnectionLost"
  implicit val m = ActorMaterializer()
  import context.system

  var requests = Map[Int, Response ⇒ Unit]()

  val tcpFlow = Flow[ByteString].via(Tcp().outgoingConnection(host, port))
  val pipelineActor = Source.actorRef(1, OverflowStrategy.fail).via(tcpFlow).to(Sink.actorRef(self, ConnectionLost)).run()

  override def receive = {
    case Msg(req, f) ⇒
      val bs = ByteString(MsgpackCodec[Request].toBytes(req, new Msgpack07Packer))
      requests += req.id → f
      pipelineActor ! bs

    case n: Notification ⇒
      val bs = ByteString(MsgpackCodec[Notification].toBytes(n, new Msgpack07Packer))
      pipelineActor ! bs

    case resp: ByteString ⇒
      val unpacker = Msgpack07Unpacker.defaultUnpacker(resp.toArray)
      MsgpackCodec[Response].unpackAndClose(unpacker) match {
          case scalaz.-\/(_) ⇒
            // if it is not a Response it can only be a Notification
            val unpacker = Msgpack07Unpacker.defaultUnpacker(resp.toArray)
            MsgpackCodec[Notification].unpackAndClose(unpacker) match {
              case scalaz.-\/(e) ⇒
                system.log.error(e, "Couldn't unpack response")

              case scalaz.\/-(notification) ⇒
                system.log.debug("retrieved notification: " + notification)
                notificationHandler(notification)
            }

          case scalaz.\/-(resp) ⇒
            system.log.debug("retrieved response: " + resp)
            requests.get(resp.id) match {
              case Some(f) ⇒
                f(resp)
              case None ⇒
                system.log.warning(s"The following response is ignored because its ID '${resp.id}' is unexpected: $resp")
            }
      }

    case ConnectionLost ⇒
      system.log.warning(s"Connection to $host:$port lost")
  }
}

private final class IdGenerator {

  private val id = new AtomicInteger(0)

  def nextId(): Int = id.getAndIncrement
}

final class InvalidResponse(msg: String) extends RuntimeException(msg)
