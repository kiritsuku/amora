package nvim

import java.util.concurrent.atomic.AtomicInteger

import scala.collection.immutable
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Sink
import akka.stream.scaladsl.Source
import akka.stream.scaladsl.Tcp
import akka.util.ByteString
import msgpack4z.Msgpack07Packer
import msgpack4z.Msgpack07Unpacker
import msgpack4z.MsgpackCodec
import msgpack4z.MsgpackUnion
import nvim.internal.Notification
import nvim.internal.Request
import nvim.internal.Response

final class Connection(host: String, port: Int)(implicit system: ActorSystem) {

  private val tcpFlow = Tcp().outgoingConnection(host, port)

  private val gen = new IdGenerator

  def sendNotification[A]
      (method: String, params: MsgpackUnion*)
      : Unit = {
    val ps = MsgpackUnion.array(params.toList)
    val req = Notification(2, method, ps)

    val bytes = MsgpackCodec[Notification].toBytes(req, new Msgpack07Packer)
    val byteString = bytes.to[immutable.IndexedSeq] map (ByteString(_))

    implicit val m = ActorMaterializer()

    val sink = Sink.onComplete {
      case Success(_) => system.log.debug(s"sent notification: $req")
      case Failure(f) => system.log.error(f, s"Couldn't send notification $req")
    }
    Source(byteString).via(tcpFlow).to(sink).run()
  }

  def sendRequest[A]
      (command: String, params: MsgpackUnion*)
      (converter: PartialFunction[MsgpackUnion, A])
      (implicit ec: ExecutionContext)
      : Future[A] = {
    val id = gen.nextId()
    val ps = MsgpackUnion.array(params.toList)
    val req = Request(0, id, command, ps)

    implicit val m = ActorMaterializer()

    val bytes = MsgpackCodec[Request].toBytes(req, new Msgpack07Packer)
    val byteString = bytes.to[immutable.IndexedSeq] map (ByteString(_))
    val resp = Source(byteString).via(tcpFlow).runFold(ByteString.empty)(_++_)

    val p = Promise[A]

    resp onComplete {
      case Success(resp) =>
        val unpacker = Msgpack07Unpacker.defaultUnpacker(resp.toArray)

        MsgpackCodec[Response].unpackAndClose(unpacker) match {
          case scalaz.-\/(e) =>
            p.failure(e)

          case scalaz.\/-(resp) =>
            system.log.debug("retrieved response: " + resp)
            if (!converter.isDefinedAt(resp.result))
              p.failure(new UnexpectedResponse(resp.result.toString))
            else
              Try(converter(resp.result)) match {
                case Success(value) => p.success(value)
                case Failure(f) => p.failure(f)
              }
        }

      case Failure(e) =>
        p.failure(e)
    }

    p.future
  }
}

final class IdGenerator {

  private val id = new AtomicInteger(0)

  def nextId(): Int = id.getAndIncrement
}