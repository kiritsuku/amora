package nvim

import java.io.InputStream
import java.io.OutputStream
import java.net.Socket

import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.Future
import scala.concurrent.Promise
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import org.msgpack.core.MessagePack
import org.msgpack.core.MessageUnpacker

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.stream.ActorMaterializer
import akka.util.ByteString
import msgpack4z._
import nvim.internal._

/**
 * Manages the connection to Neovim. Communication is done through the
 * msgpack-rpc protocol, whose spec is here:
 *
 * [[https://github.com/msgpack-rpc/msgpack-rpc/blob/master/spec.md]]
 */
final class Connection(host: String, port: Int)(implicit system: ActorSystem) {

  private var notificationHandlers = List[Notification ⇒ Unit]()
  private val notifyHandlers: Notification ⇒ Unit = { n ⇒
    notificationHandlers foreach (_(n))
  }

  private val connectionActor = system.actorOf(Props(classOf[ConnectionActor], host, port, notifyHandlers))
  private val gen = new IdGenerator

  // I don't know any better than to delay the creation of a new connection.
  // If we don't do this, we can't immediately after startup of the connection
  // send messages because no downstream demand is yet registered. The solution
  // would be to wait until this demand is registered but I don't know how to find
  // out when it gets registered. Therefore we just wait a little bit and hope
  // that the delay is enough.
  Thread.sleep(100)

  /**
   * Adds a new notification handler and notifies it for every incoming
   * notification event. If the handler is already added, it is not added again.
   */
  def addNotificationHandler(handler: Notification ⇒ Unit): Unit = {
    if (!notificationHandlers.contains(handler))
      notificationHandlers +:= handler
  }

  /**
   * Removes the notification handler from the list of registered handlers.
   */
  def removeNotificationHandler(handler: Notification ⇒ Unit): Unit = {
    notificationHandlers = notificationHandlers.filterNot(_ == handler)
  }

  /**
   * Sends a [[nvim.internal.Notification]] to Nvim.
   */
  def sendNotification[A]
      (method: String, params: MsgpackUnion*)
      : Unit = {
    val ps = MsgpackUnion.array(params.toList)
    val req = Notification(2, method, ps)
    connectionActor ! req
  }

  /**
   * Sends a [[nvim.internal.Request]] to Nvim and returns the value that is in
   * the response. `converter` is used to convert the data that is sent over the
   * wire into the actual value that is expected.
   */
  def sendRequest[A]
      (command: String, params: MsgpackUnion*)
      (converter: PartialFunction[MsgpackUnion, A])
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
    connectionActor ! SendRequest(req, f)

    p.future
  }
}

private final case class SendRequest(req: Request, f: Response ⇒ Unit)

private final class SocketConnection(host: String, port: Int) {

  private val socket = new Socket(host, port)

  def inputStream: InputStream =
    socket.getInputStream

  def outputStream: OutputStream =
    socket.getOutputStream

  def close(): Unit =
    socket.close()
}

private final class ConnectionActor(host: String, port: Int, notificationHandler: Notification ⇒ Unit) extends Actor {
  val ConnectionLost = "ConnectionLost"
  implicit val m = ActorMaterializer()
  import context.system

  var requests = Map[Int, Response ⇒ Unit]()

  val ResponseId = 1
  val NotificationId = 2

  val conn = new SocketConnection(host, port)

  val thread = new Thread(new Runnable {
    override def run() = {
      val unp = MessagePack.DEFAULT.newUnpacker(conn.inputStream)
      val unpacker = new Msgpack07Unpacker(unp)
      readResp(unp, unpacker)

      system.log.warning(s"Connection to $host:$port lost")
    }
  })
  thread.start()

  def readResp(unp: MessageUnpacker, unpacker: Msgpack07Unpacker): Unit = {
    /*
     * The stupid API doesn't allow to do a look ahead, therefore we have to
     * use reflection to correct the internal position in the buffer.
     */
    def lookAheadType(): Int = {
      unpacker.unpackArrayHeader()
      val tpe = unpacker.unpackInt()
      val f = classOf[MessageUnpacker].getDeclaredField("position")
      f.setAccessible(true)
      val pos = f.getInt(unp)
      f.setInt(unp, pos-2)
      tpe
    }

    lookAheadType() match {
      case ResponseId ⇒
        MsgpackCodec[Response].unpack(unpacker) match {
          case scalaz.-\/(e) ⇒
            system.log.error(e, "Couldn't unpack response")

          case scalaz.\/-(resp) ⇒
            system.log.debug(s"retrieved: $resp")
            requests.get(resp.id) match {
              case Some(f) ⇒
                f(resp)
              case None ⇒
                system.log.warning(s"The following response is ignored because its ID '${resp.id}' is unexpected: $resp")
            }
        }

      case NotificationId ⇒
        MsgpackCodec[Notification].unpack(unpacker) match {
          case scalaz.-\/(e) ⇒
            system.log.error(e, "Couldn't unpack response")

          case scalaz.\/-(notification) ⇒
            system.log.debug(s"retrieved: $notification")
            notificationHandler(notification)
        }

      case tpe ⇒
        system.log.error(new Throwable, s"Unknow type: $tpe")
    }

    if (unp.hasNext())
      readResp(unp, unpacker)
  }

  override def receive = {
    case SendRequest(req, f) ⇒
      val bytes = MsgpackCodec[Request].toBytes(req, new Msgpack07Packer)
      requests += req.id → f
      system.log.debug(s"sending: $req")
      val out = conn.outputStream
      out.write(bytes)
      out.flush()

    case n: Notification ⇒
      val bytes = MsgpackCodec[Notification].toBytes(n, new Msgpack07Packer)
      system.log.debug(s"sending: $n")
      val out = conn.outputStream
      out.write(bytes)
      out.flush()

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
                system.log.debug(s"retrieved: $notification")
                notificationHandler(notification)
            }

          case scalaz.\/-(resp) ⇒
            system.log.debug(s"retrieved: $resp")
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
