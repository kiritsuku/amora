package backend

import java.nio.ByteBuffer

import scala.concurrent.Future
import scala.concurrent.duration._

import akka.NotUsed
import akka.actor.ActorSystem
import akka.actor.Props
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.Flow
import akka.stream.scaladsl.Sink
import akka.stream.scaladsl.Source
import akka.util.Timeout
import backend.actors.NvimActor
import backend.actors.NvimMsg
import backend.actors.QueueActor
import backend.actors.QueueMsg
import protocol._

final class BackendSystem(implicit system: ActorSystem)
    extends AnyRef
    with IndexerSystem {

  import boopickle.Default._
  import NvimMsg._

  private val nvim = system.actorOf(Props[NvimActor])
  private val queue = system.actorOf(Props[QueueActor])

  def addQueueItem(func: Logger ⇒ Unit): Future[Int] = {
    import akka.pattern.ask
    implicit val timeout = Timeout(5.seconds)
    queue.ask(QueueMsg.Add(func)).asInstanceOf[Future[Int]]
  }

  def authFlow(): Flow[ByteBuffer, ByteBuffer, NotUsed] = {
    val out = Source
      .actorRef[Response](1, OverflowStrategy.fail)
      .mapMaterializedValue { nvim ! NewClient(_) }
      .map(Pickle.intoBytes(_))
    Flow.fromSinkAndSource(Sink.ignore, out)
  }

  def messageFlow(sender: String): Flow[ByteBuffer, ByteBuffer, NotUsed] = {
    def sink(sender: String) = Sink.actorRef[NvimMsg](nvim, ClientLeft(sender))

    val in = Flow[ByteBuffer]
      .map(b ⇒ ReceivedMessage(sender, Unpickle[Request].fromBytes(b)))
      .to(sink(sender))
    val out = Source
      .actorRef[Response](1, OverflowStrategy.fail)
      .mapMaterializedValue { nvim ! ClientReady(sender, _) }
      .map(Pickle.intoBytes(_))
    Flow.fromSinkAndSource(in, out)
  }
}
